#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [converter? (-> any/c boolean?)]
  [make-converter
   (->* ((-> any/c any/c) (-> any/c any/c))
        (#:name (or/c interned-symbol? #false))
        converter?)]
  [convert-forward (-> converter? any/c any/c)]
  [convert-backward (-> converter? any/c any/c)]
  [converter/c (-> contract? contract? contract?)]
  [identity-converter converter?]
  [number<->string (converter/c number? string?)]
  [string<->symbol (converter/c string? symbol?)]
  [string<->keyword (converter/c string? keyword?)]
  [symbol<->keyword (converter/c symbol? keyword?)]
  [converter-pipe (-> converter? ... converter?)]
  [converter-flip
   (->* (converter?) (#:name (or/c interned-symbol? #false)) converter?)]))

(require guard
         racket/bool
         racket/contract/combinator
         racket/keyword
         racket/symbol
         rebellion/base/symbol
         rebellion/collection/list
         rebellion/private/contract-projection
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/type/object
         (only-in rebellion/base/immutable-string immutable-string? number->immutable-string))

(module+ test
  (require (submod "..")
           racket/contract/parametric
           racket/contract/region
           racket/function
           rackunit))

;@------------------------------------------------------------------------------
;; Core API

(define-object-type converter (forward-function backward-function)
  #:constructor-name constructor:converter)


(define (make-converter forward-function backward-function #:name [name #false])
  (constructor:converter
   #:forward-function (fix-arity forward-function)
   #:backward-function (fix-arity backward-function)
   #:name name))

(define (fix-arity conversion-function)
  (if (equal? (procedure-arity conversion-function) 1)
      conversion-function
      (procedure-reduce-arity conversion-function 1)))

(define (convert-forward converter domain-value)
  ((converter-forward-function converter) domain-value))

(define (convert-backward converter range-value)
  ((converter-backward-function converter) range-value))

(define/name identity-converter
  (make-converter values values #:name enclosing-variable-name))

;@------------------------------------------------------------------------------
;; Contracts

(define (converter-impersonate
         converter
         #:domain-input-guard [domain-input-guard #false]
         #:domain-output-guard [domain-output-guard #false]
         #:range-input-guard [range-input-guard #false]
         #:range-output-guard [range-output-guard #false]
         #:properties [properties (hash)]
         #:forward-marks [forward-marks (hash)]
         #:backward-marks [backward-marks (hash)]
         #:chaperone?
         [chaperone?
          (nor domain-input-guard
               domain-output-guard
               range-input-guard
               range-output-guard)])
  (define forward-function (converter-forward-function converter))
  (define backward-function (converter-backward-function converter))
  (define impersonated-forward-function
    (function-impersonate
     forward-function
     #:arguments-guard domain-input-guard
     #:results-guard range-output-guard
     #:application-marks forward-marks
     #:chaperone? chaperone?))
  (define impersonated-backward-function
    (function-impersonate
     backward-function
     #:arguments-guard range-input-guard
     #:results-guard domain-output-guard
     #:application-marks backward-marks
     #:chaperone? chaperone?))
  (define impersonated-without-props
    (make-converter impersonated-forward-function impersonated-backward-function
                    #:name (object-name converter)))
  (object-impersonate impersonated-without-props descriptor:converter
                      #:properties properties))

(define/name (converter/c domain-contract* range-contract*)
  (define domain-contract
    (coerce-contract enclosing-function-name domain-contract*))
  (define range-contract
    (coerce-contract enclosing-function-name range-contract*))
  (define contract-name
    (build-compound-type-name
     enclosing-function-name domain-contract range-contract))
  (define domain-projection (contract-late-neg-projection domain-contract))
  (define range-projection (contract-late-neg-projection range-contract))
  (define chaperone?
    (and (chaperone-contract? domain-contract)
         (chaperone-contract? range-contract)))
  (define (projection blame)
    (define domain-input-blame
      (blame-add-context blame "an input in the converter domain of"
                         #:swap? #true))
    (define domain-output-blame
      (blame-add-context blame "an output in the converter domain of"))
    (define range-output-blame
      (blame-add-context blame "an output in the converter range of"))
    (define range-input-blame
      (blame-add-context
       blame "an input in the converter range of" #:swap? #true))
    (define late-neg-domain-input-guard (domain-projection domain-input-blame))
    (define late-neg-range-output-guard (range-projection range-output-blame))
    (define late-neg-range-input-guard (range-projection range-input-blame))
    (define late-neg-domain-output-guard
      (domain-projection domain-output-blame))
    (λ (original-converter missing-party)
      (assert-satisfies original-converter converter? blame
                        #:missing-party missing-party)
      (define props
        (hash impersonator-prop:contracted the-contract
              impersonator-prop:blame (cons blame missing-party)))
      (define (domain-input-guard input)
        (late-neg-domain-input-guard input missing-party))
      (define (domain-output-guard output)
        (late-neg-domain-output-guard output missing-party))
      (define (range-input-guard input)
        (late-neg-range-input-guard input missing-party))
      (define (range-output-guard output)
        (late-neg-range-output-guard output missing-party))
      (converter-impersonate
       original-converter
       #:domain-input-guard domain-input-guard
       #:domain-output-guard domain-output-guard
       #:range-input-guard range-input-guard
       #:range-output-guard range-output-guard
       #:chaperone? chaperone?
       #:properties props)))
  (define the-contract
    ((if chaperone? make-chaperone-contract make-contract)
     #:name contract-name
     #:first-order converter?
     #:late-neg-projection projection))
  the-contract)

(module+ test
  (test-case (name-string converter/c)

    (test-case "should make chaperones for non-impersonator operand contracts"
      (check-pred chaperone-contract? (converter/c any/c any/c))
      (check-pred chaperone-contract? (converter/c string? string?))
      (check-pred chaperone-contract?
                  (converter/c (-> any/c any/c) (-> any/c any/c)))
      (check-pred (negate chaperone-contract?) (converter/c (new-∀/c) any/c))
      (check-pred (negate chaperone-contract?) (converter/c any/c (new-∀/c))))

    (define/name number<->symbol
      (make-converter
       (λ (x) (string->symbol (number->string x)))
       (λ (sym) (string->number (symbol->string sym)))
       #:name enclosing-variable-name))

    (test-case "should only enforce converter? predicate in first order checks"
      (check-exn
       exn:fail:contract:blame?
       (λ () (invariant-assertion (converter/c any/c any/c) 42)))
      (check-not-exn
       (λ () (invariant-assertion (converter/c any/c any/c) number<->symbol)))
      (check-not-exn
       (λ ()
         (invariant-assertion (converter/c number? symbol?) number<->symbol)))
      (check-not-exn
       (λ ()
         (invariant-assertion (converter/c string? string?) number<->symbol))))
    
    (test-case "should enforce domain contract on inputs"
      (define/contract contracted (converter/c number? any/c) number<->symbol)
      (check-not-exn (λ () (convert-forward contracted 42)))
      (check-exn
       #rx"contracted: contract violation"
       (λ () (convert-forward contracted "foo")))
      (check-exn #rx"\"foo\"" (λ () (convert-forward contracted "foo")))
      (check-exn #rx"number\\?" (λ () (convert-forward contracted "foo")))
      (check-exn #rx"input" (λ () (convert-forward contracted "foo")))
      (check-exn #rx"domain" (λ () (convert-forward contracted "foo")))
      (check-exn #rx"expected" (λ () (convert-forward contracted "foo")))
      (check-exn #rx"given" (λ () (convert-forward contracted "foo"))))

    (test-case "should enforce domain contract on outputs"
      (define/contract contracted (converter/c integer? any/c) number<->symbol)
      (check-not-exn (λ () (convert-backward contracted '|42|)))
      (check-exn
       #rx"contracted: broke its own contract"
       (λ () (convert-backward contracted '|42.5|)))
      (check-exn #rx"42\\.5" (λ () (convert-backward contracted '|42.5|)))
      (check-exn #rx"integer\\?" (λ () (convert-backward contracted '|42.5|)))
      (check-exn #rx"output" (λ () (convert-backward contracted '|42.5|)))
      (check-exn #rx"domain" (λ () (convert-backward contracted '|42.5|)))
      (check-exn #rx"promised" (λ () (convert-backward contracted '|42.5|)))
      (check-exn #rx"produced" (λ () (convert-backward contracted '|42.5|))))

    (test-case "should enforce range contract on inputs"
      (define/contract contracted (converter/c any/c symbol?) number<->symbol)
      (check-not-exn (λ () (convert-backward contracted '|42|)))
      (check-exn
       #rx"contracted: contract violation"
       (λ () (convert-backward contracted "foo")))
      (check-exn #rx"\"foo\"" (λ () (convert-backward contracted "foo")))
      (check-exn #rx"symbol\\?" (λ () (convert-backward contracted "foo")))
      (check-exn #rx"input" (λ () (convert-backward contracted "foo")))
      (check-exn #rx"range" (λ () (convert-backward contracted "foo")))
      (check-exn #rx"expected" (λ () (convert-backward contracted "foo")))
      (check-exn #rx"given" (λ () (convert-backward contracted "foo"))))

    (test-case "should enforce range contract on outputs"
      (define (short-symbol? v)
        (and (symbol? v) (< (string-length (symbol->immutable-string v)) 4)))
      (define/contract contracted (converter/c any/c short-symbol?)
        number<->symbol)
      (check-not-exn (λ () (convert-forward contracted 42)))
      (check-exn
       #rx"contracted: broke its own contract"
       (λ () (convert-forward contracted 999999)))
      (check-exn #rx"999999" (λ () (convert-forward contracted 999999)))
      (check-exn
       #rx"short\\-symbol\\?" (λ () (convert-forward contracted 999999)))
      (check-exn #rx"output" (λ () (convert-forward contracted 999999)))
      (check-exn #rx"range" (λ () (convert-forward contracted 999999)))
      (check-exn #rx"promised" (λ () (convert-forward contracted 999999)))
      (check-exn #rx"produced" (λ () (convert-forward contracted 999999))))

    (test-case "should add contract system impersonator properties"
      (define the-contract (converter/c number? symbol?))
      (define/contract contracted the-contract number<->symbol)
      (check-pred has-contract? contracted)
      (check-equal? (value-contract contracted) the-contract)
      (check-pred has-blame? contracted))))

;@------------------------------------------------------------------------------
;; Built-in converters and converter utilities

(define/guard (converter-pipe #:name [name 'piped] . converters)
  (guard (nonempty-list? converters) #:else
    identity-converter)
  (define forward-functions (map converter-forward-function converters))
  (define backward-functions (map converter-backward-function converters))
  (make-converter
   (apply compose (reverse forward-functions))
   (apply compose backward-functions)
   #:name name))

(define (converter-flip converter #:name [name 'flipped])
  (make-converter (converter-backward-function converter)
                  (converter-forward-function converter)
                  #:name name))

(define/name number<->string
  (make-converter number->immutable-string string->number
                  #:name enclosing-variable-name))

(define/name string<->symbol
  (make-converter string->symbol symbol->immutable-string
                  #:name enclosing-variable-name))

(define/name string<->keyword
  (make-converter string->keyword keyword->immutable-string
                  #:name enclosing-variable-name))

(define (symbol->keyword sym)
  (string->keyword (symbol->immutable-string sym)))

(define (keyword->symbol kw)
  (string->symbol (keyword->immutable-string kw)))

(define/name symbol<->keyword
  (make-converter symbol->keyword keyword->symbol
                  #:name enclosing-variable-name))

(module+ test
  (test-case (name-string number<->string)
    (check-equal? (convert-forward number<->string 42) "42")
    (check-pred immutable-string? (convert-forward number<->string 42))
    (check-equal? (convert-backward number<->string "-5.7") -5.7)
    (check-equal? (convert-backward number<->string (make-string 3 #\1)) 111)
    (check-exn
     #rx"number<\\->string: contract violation"
     (λ () (convert-forward number<->string "42")))
    (check-exn
     #rx"number<\\->string: contract violation"
     (λ () (convert-backward number<->string 42))))

  (define (mutable-foo)
    (define s (make-string 3))
    (string-set! s 0 #\f)
    (string-set! s 1 #\o)
    (string-set! s 2 #\o)
    s)

  (define (unreadable-foo) (string->unreadable-symbol "foo"))
  (define (uninterned-foo) (string->uninterned-symbol "foo"))

  (test-case (name-string string<->symbol)
    (check-equal? (convert-forward string<->symbol "foo") 'foo)
    (check-equal? (convert-forward string<->symbol (mutable-foo)) 'foo)
    (check-equal? (convert-backward string<->symbol 'foo) "foo")
    (check-pred immutable-string? (convert-backward string<->symbol 'foo))
    (check-equal? (convert-backward string<->symbol (unreadable-foo)) "foo")
    (check-pred
     immutable-string? (convert-backward string<->symbol (unreadable-foo)))
    (check-equal? (convert-backward string<->symbol (uninterned-foo)) "foo")
    (check-pred
     immutable-string? (convert-backward string<->symbol (uninterned-foo))))

  (test-case (name-string string<->keyword)
    (check-equal? (convert-forward string<->keyword "foo") '#:foo)
    (check-equal? (convert-forward string<->keyword (mutable-foo)) '#:foo)
    (check-equal? (convert-backward string<->keyword '#:foo) "foo")
    (check-pred immutable-string? (convert-backward string<->keyword '#:foo)))

  (test-case (name-string symbol<->keyword)
    (check-equal? (convert-forward symbol<->keyword 'foo) '#:foo)
    (check-equal? (convert-forward symbol<->keyword (unreadable-foo)) '#:foo)
    (check-equal? (convert-forward symbol<->keyword (uninterned-foo)) '#:foo)
    (check-equal? (convert-backward symbol<->keyword '#:foo) 'foo))

  (test-case (name-string converter-flip)
    (define keyword<->string (converter-flip string<->keyword))
    (check-equal? (convert-forward keyword<->string '#:foo) "foo")
    (check-equal? (convert-backward keyword<->string "foo") '#:foo)
    (check-equal?
     (converter-flip keyword<->string #:name (name string<->keyword))
     string<->keyword))

  (test-case (name-string converter-pipe)
    (define number<->keyword (converter-pipe number<->string string<->keyword))
    (check-equal? (convert-forward number<->keyword 42) '#:42)
    (check-equal? (convert-backward number<->keyword '#:42) 42)))
