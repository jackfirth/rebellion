#lang racket/base

(require racket/contract/base)

(provide variant
         variant/c)

(provide
 (contract-out
  [variant? (-> any/c boolean?)]
  [variant-value (-> variant? any/c)]
  [variant-tag (-> variant? keyword?)]
  [variant-tagged-as? (-> variant? keyword? boolean?)]))

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/combinator
         racket/format
         racket/list
         racket/local
         racket/match
         racket/string
         racket/struct
         rebellion/private/contract-projection
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           racket/contract/region
           rackunit))

;@------------------------------------------------------------------------------

(define (make-variant-properties descriptor)
  (define type-name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (this) type-name)
     (λ (this)
       (define tag (accessor this 0))
       (define value (accessor this 1))
       (define tag-str (string-append "#:" (keyword->string tag)))
       (list (unquoted-printing-string tag-str) value))))
  (list (cons prop:custom-write custom-write)
        (cons prop:equal+hash (default-tuple-equal+hash descriptor))))

(define-tuple-type variant (tag value)
  #:omit-root-binding
  #:property-maker make-variant-properties)

(define (variant-keyword-function kws kw-args)
  (when (> (length kws) 1)
    (raise-arguments-error 'variant
                           "multiple keyword arguments"
                           "keywords" kws
                           "values" kw-args))
  (when (< (length kws) 1)
    (raise-arguments-error 'variant "no arguments given"))
  (constructor:variant (first kws) (first kw-args)))

(define unchecked:variant
  (procedure-reduce-keyword-arity
   (make-keyword-procedure variant-keyword-function)
   0
   empty
   #false))

(define-module-boundary-contract contracted:variant unchecked:variant
  (unconstrained-domain-> variant?)
  #:name-for-blame variant)

(define-match-expander variant
  (syntax-parser
    [(_ tag:keyword value-pattern)
     #'(? variant?
          (app variant-tag 'tag)
          (app variant-value value-pattern))])
  (make-rename-transformer #'contracted:variant))

(define (variant-tagged-as? var tag-keyword)
  (equal? (variant-tag var) tag-keyword))

(module+ test
  (test-case "basic variant construction"
    (define var (variant #:success 42))
    (check-equal? (variant-tag var) '#:success)
    (check-equal? (variant-value var) 42)
    (check-equal? var (variant #:success 42)))

  (test-case "variant pattern matching"
    (check-match (variant #:success 42) (variant #:success 42))
    (check-match (variant #:success 42) (not (variant #:success 0)))
    (check-match (variant #:success 42) (not (variant #:failure _))))

  (test-case (name-string variant-tagged-as?)
    (check-true (variant-tagged-as? (variant #:success 42) '#:success))
    (check-false (variant-tagged-as? (variant #:failure "oh no") '#:success))))

;@------------------------------------------------------------------------------
;; Contracts

(define (make-variant-contract case-keywords uncoerced-case-contracts)
  (define case-contracts
    (coerce-contracts (name variant/c) uncoerced-case-contracts))
  (define ctc-name (build-variant-contract-name case-keywords case-contracts))
  ((cond [(andmap flat-contract? case-contracts) make-flat-contract]
         [(andmap chaperone-contract? case-contracts) make-chaperone-contract]
         [else make-contract])
   #:name ctc-name
   #:first-order
   (build-variant-contract-first-order-test case-keywords case-contracts)
   #:late-neg-projection
   (build-variant-contract-projection case-keywords case-contracts)))

(define (build-variant-contract-name case-keywords case-contracts)
  (cons (name variant/c)
        (for/list ([kw (in-list case-keywords)]
                   [c (in-list case-contracts)]
                   #:when #true
                   [name-part (in-list (list kw (contract-name c)))])
          name-part)))

(define (build-variant-contract-projection case-keywords case-contracts)
  (define case-projections (map contract-late-neg-projection case-contracts))
  (λ (blame)
    (define late-neg-case-guards
      (for/list ([proj (in-list case-projections)]
                 [kw (in-list case-keywords)])
        (proj (blame-add-context blame (format "the ~a case of" kw)))))
    (λ (v missing-party)
      (assert-satisfies v variant? blame #:missing-party missing-party)
      (define tag
        (assert-variant-tag v case-keywords blame
                            #:missing-party missing-party))
      (define case-index (index-of case-keywords tag))
      (define case-guard (list-ref late-neg-case-guards case-index))
      (constructor:variant tag (case-guard (variant-value v) missing-party)))))

(define (build-variant-contract-first-order-test case-keywords case-contracts)
  (define first-order-case-tests (map contract-first-order case-contracts))
  (λ (first-order-test v)
    (guarded-block
      (guard (variant? v) else #false)
      (define index (index-of case-keywords (variant-tag v)))
      (and index ((list-ref first-order-case-tests index) v)))))

(define (assert-variant-tag var tags blame #:missing-party missing-party)
  (define tail (member (variant-tag var) tags))
  (unless tail
    (define last-separator (if (equal? (length tags) 2) " or " ", or "))
    (raise-blame-error blame #:missing-party missing-party var
                       '(expected: "a variant with tag ~a" given: "~e")
                       (string-join (map ~a tags) ", "
                                    #:before-last last-separator)
                       var))
  (first tail))

(define variant/c
  ;; The name of the keyword procedure is based on the second function argument,
  ;; so we locally define it with the name "variant/c" to ensure the procedure
  ;; gets the correct name.
  (local [(define (variant/c) (make-variant-contract '() '()))]
    (make-keyword-procedure make-variant-contract variant/c)))

(module+ test
  (test-case (name-string variant/c)

    (test-case "should build a reasonably-named contract"
      (define contract (variant/c #:case1 (-> any/c any/c) #:case2 number?))
      (define expected '(variant/c #:case1 (-> any/c any/c) #:case2 number?))
      (check-equal? (contract-name contract) expected))

    (test-case "should reject non-variant values"
      (define/contract (bad) (-> (variant/c #:foo any/c)) 42)
      (check-exn exn:fail:contract:blame? bad)
      (check-exn #rx"bad: broke its own contract" bad)
      (check-exn #rx"promised: variant\\?" bad)
      (check-exn #rx"produced: 42" bad))

    (test-case "should only allow variants with one of the given tags"
      (define/contract (bad)
        (-> (variant/c #:red any/c #:blue any/c #:green any/c))
        (variant #:white 42))
      (check-exn exn:fail:contract:blame? bad)
      (check-exn #rx"bad: broke its own contract" bad)
      (check-exn
       #rx"promised: a variant with tag #:blue, #:green, or #:red" bad)
      (check-exn #rx"produced: \\(variant #:white 42\\)" bad))

    (test-case "should enforce the relevant case contract"
      (define contract (variant/c #:success number? #:failure string?))
      (define/contract try (-> contract void?) void)
      (define (try-success v) (try (variant #:success v)))
      (define (try-failure v) (try (variant #:failure v)))
      (check-not-exn (λ () (try-success 42)))
      (check-not-exn (λ () (try-failure "kaboom!")))
      (check-exn exn:fail:contract:blame? (λ () (try-failure 42)))
      (check-exn #rx"expected: string\\?" (λ () (try-failure 42)))
      (check-exn #rx"given: 42" (λ () (try-failure 42)))
      (check-exn #rx"the #:failure case of" (λ () (try-failure 42))))))
