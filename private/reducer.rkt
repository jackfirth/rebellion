#lang racket/base

(require racket/contract/base)

(provide
 for/reducer
 for*/reducer
 (contract-out
  [reducer? predicate/c]
  [reducer/c (-> contract? contract? contract?)]
  [make-fold-reducer
   (->* ((-> any/c any/c any/c) any/c)
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [make-effectful-fold-reducer
   (->* ((-> any/c any/c any/c) (-> any/c) (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [make-reducer
   (->* (#:starter (-> reduction-state/c)
         #:consumer (-> any/c any/c reduction-state/c)
         #:finisher (-> any/c any/c)
         #:early-finisher (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        reducer?)]
  [reducer-starter (-> reducer? (-> reduction-state/c))]
  [reducer-consumer (-> reducer? (-> any/c any/c reduction-state/c))]
  [reducer-finisher (-> reducer? (-> any/c any/c))]
  [reducer-early-finisher (-> reducer? (-> any/c any/c))]
  [reduce (-> reducer? any/c ... any/c)]
  [reduce-all (-> reducer? sequence? any/c)]
  [into-sum (reducer/c number? number?)]
  [into-product (reducer/c number? number?)]
  [into-count (reducer/c any/c number?)]
  [into-first (reducer/c any/c option?)]
  [into-last (reducer/c any/c option?)]
  [into-nth (-> natural? (reducer/c any/c option?))]
  [into-index-of (-> any/c (reducer/c any/c (option/c natural?)))]
  [into-index-where (-> predicate/c (reducer/c any/c (option/c natural?)))]
  [into-any-match? (-> predicate/c (reducer/c any/c boolean?))]
  [into-all-match? (-> predicate/c (reducer/c any/c boolean?))]
  [into-none-match? (-> predicate/c (reducer/c any/c boolean?))]
  [into-for-each (-> (-> any/c void?) (reducer/c any/c void?))]
  [into-max
   (->* () (comparator? #:key (-> any/c any/c)) (reducer/c any/c option?))]
  [into-min
   (->* () (comparator? #:key (-> any/c any/c)) (reducer/c any/c option?))]
  [into-string (reducer/c char? immutable-string?)]
  [into-line (reducer/c char? immutable-string?)]
  [join-into-string
   (->* (immutable-string?)
        (#:before-first immutable-string?
         #:before-last immutable-string?
         #:after-last immutable-string?)
        (reducer/c immutable-string? immutable-string?))]
  [reducer-impersonate
   (->* (reducer?)
        (#:domain-guard (or/c (-> any/c any/c) #f)
         #:range-guard (or/c (-> any/c any/c) #f)
         #:properties impersonator-property-hash/c
         #:chaperone? boolean?)
        reducer?)]
  [reducer-map
   (->* (reducer?)
        (#:domain (-> any/c any/c) #:range (-> any/c any/c))
        reducer?)]
  [reducer-filter (-> reducer? predicate/c reducer?)]
  [reducer-limit (-> reducer? natural? reducer?)]))

(require (for-syntax racket/base
                     racket/contract/base
                     syntax/parse)
         racket/bool
         racket/contract/combinator
         racket/list
         racket/math
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/symbol
         rebellion/base/variant
         rebellion/private/contract-projection
         rebellion/private/guarded-block
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/type/record
         rebellion/type/object
         rebellion/type/wrapper
         syntax/parse/define)

(begin-for-syntax
  (provide
   (contract-out
    [make-reducer-based-for-comprehensions
     (-> syntax? (values (-> syntax? syntax?) (-> syntax? syntax?)))])))

(module+ test
  (require (submod "..")
           racket/contract/region
           racket/set
           rackunit))

;@------------------------------------------------------------------------------
;; Core APIs

(define reduction-state/c (variant/c #:consume any/c #:early-finish any/c))

(define-object-type reducer (starter consumer finisher early-finisher)
  #:constructor-name constructor:reducer)

(define (make-reducer #:starter starter*
                      #:consumer consumer*
                      #:finisher finisher*
                      #:early-finisher early-finisher*
                      #:name [name #f])
  (define starter
    (if (zero? (procedure-arity starter*))
        starter*
        (procedure-reduce-arity starter* 0)))
  (define consumer
    (if (equal? (procedure-arity consumer*) 2)
        consumer*
        (procedure-reduce-arity consumer* 2)))
  (define finisher
    (if (equal? (procedure-arity finisher*) 1)
        finisher*
        (procedure-reduce-arity finisher* 1)))
  (define early-finisher
    (if (equal? (procedure-arity early-finisher*) 1)
        early-finisher*
        (procedure-reduce-arity early-finisher* 1)))
  (constructor:reducer #:starter starter
                       #:consumer consumer
                       #:finisher finisher
                       #:early-finisher early-finisher
                       #:name name))

(define (make-fold-reducer consumer init-state #:name [name #f])
  (make-effectful-fold-reducer consumer (λ () init-state) values #:name name))

(define (make-effectful-fold-reducer consumer init-state-maker finisher
                                     #:name [name #f])
  (make-reducer #:starter (λ () (variant #:consume (init-state-maker)))
                #:consumer (λ (s v) (variant #:consume (consumer s v)))
                #:finisher finisher
                #:early-finisher values
                #:name name))

(define/name into-sum (make-fold-reducer + 0 #:name enclosing-variable-name))

(define/name into-product
  (make-fold-reducer * 1 #:name enclosing-variable-name))

(define/name into-count
  (make-fold-reducer (λ (s _) (add1 s)) 0 #:name enclosing-variable-name))

(define (reduce red . vs)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (define/guard (loop [tagged-state (starter)] [vs vs])
    (define state (variant-value tagged-state))
    (guard (variant-tagged-as? tagged-state '#:early-finish) then
      (early-finisher state))
    (guard (empty? vs) else (loop (consumer state (first vs)) (rest vs)))
    (finisher state))
  (loop))

(define/name (reduce-all red seq)
  (define starter (reducer-starter red))
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (define-values (first-vs generate-rest) (sequence-generate* seq))
  (define/guard (loop [tagged-state (starter)]
                      [sequence-position 0]
                      [first-vs first-vs]
                      [generate-rest generate-rest])
    (define state (variant-value tagged-state))
    (guard (variant-tagged-as? tagged-state '#:early-finish) then
      (early-finisher state))
    (guard (false? first-vs) then (finisher state))
    (guard (equal? (length first-vs) 1) else
      (apply raise-result-arity-error
             enclosing-function-name 1
             (format "\n  in: sequence elements at position ~a"
                     sequence-position)
             first-vs))
    (define-values (next-vs next-generate-rest) (generate-rest))
    (loop (consumer state (first first-vs))
          (add1 sequence-position)
          next-vs
          next-generate-rest))
  (loop))

(module+ test
  (test-case (name-string reduce)
    (check-equal? (reduce into-sum 1 2 3 4 5) 15)
    (check-equal? (reduce into-product 2 3 5 7) 210)
    (check-equal? (reduce into-count 'a 'b 'c 'd) 4)
    (check-equal? (reduce into-sum) 0)
    (check-equal? (reduce into-product) 1)
    (check-equal? (reduce into-count) 0))
  (test-case (name-string reduce-all)
    (check-equal? (reduce-all into-sum (in-range 6)) 15)
    (check-equal? (reduce-all into-product
                              (in-hash-values (hash 'a 2 'b 3 'c 5 'd 7)))
                  210)
    (check-equal? (reduce-all into-count (in-string "abcde")) 5)))

;@------------------------------------------------------------------------------
;; Contracts

(define ((reducer-consumer-guard domain-guard) state element)
  (values state (domain-guard element)))

(define (reducer-impersonate
         reducer
         #:domain-guard [domain-guard #f]
         #:range-guard [range-guard #f]
         #:properties [properties (hash)]
         #:chaperone?
         [chaperone? (and (false? domain-guard) (false? range-guard))])
  (define consumer (reducer-consumer reducer))
  (define finisher (reducer-finisher reducer))
  (define early-finisher (reducer-early-finisher reducer))
  (define domain-chaperone? (or chaperone? (false? domain-guard)))
  (define range-chaperone? (or chaperone? (false? range-guard)))

  (define impersonated-consumer
    (function-impersonate
     consumer
     #:arguments-guard (and domain-guard (reducer-consumer-guard domain-guard))
     #:chaperone? domain-chaperone?))

  (define impersonated-finisher
    (function-impersonate finisher
                          #:results-guard range-guard
                          #:chaperone? range-chaperone?))
  
  (define impersonated-early-finisher
    (function-impersonate early-finisher
                          #:results-guard range-guard
                          #:chaperone? range-chaperone?))
  
  (define impersonated-without-props
    (make-reducer #:starter (reducer-starter reducer)
                  #:consumer impersonated-consumer
                  #:finisher impersonated-finisher
                  #:early-finisher impersonated-early-finisher
                  #:name (object-name reducer)))
  
  (object-impersonate impersonated-without-props descriptor:reducer
                      #:properties properties))

(module+ test
  (test-case (name-string reducer-impersonate)
    (test-case "properties only"
      (define reducer (into-all-match? even?))
      (define properties (hash impersonator-prop:contracted 'foo))
      (define impersonated
        (reducer-impersonate reducer #:properties properties))
      (check-equal? (value-contract impersonated) 'foo)
      (check-equal? impersonated reducer)
      (check impersonator-of? impersonated reducer)
      (check impersonator-of? reducer impersonated)
      (check chaperone-of? impersonated reducer)
      (check chaperone-of? reducer impersonated))
    
    (test-case "domain guard"
      (define counter (box 0))
      (define (guard v)
        (set-box! counter (add1 (unbox counter)))
        v)
      (define impersonated
        (reducer-impersonate into-list #:domain-guard guard #:chaperone? #t))
      (check-equal? (reduce impersonated 'a 'b 'c) (list 'a 'b 'c))
      (check-equal? (unbox counter) 3)
      (check-equal? impersonated into-list)
      (check impersonator-of? impersonated into-list)
      (check-false (impersonator-of? into-list impersonated))
      (check chaperone-of? impersonated into-list)
      (check-false (chaperone-of? into-list impersonated)))

    (test-case "range guard"
      (define result (box #f))
      (define (guard v)
        (set-box! result v)
        v)
      (define impersonated
        (reducer-impersonate into-list #:range-guard guard #:chaperone? #t))
      (check-equal? (reduce impersonated 1 2 3) (list 1 2 3))
      (check-equal? (unbox result) (list 1 2 3))
      (check-equal? impersonated into-list)
      (check impersonator-of? impersonated into-list)
      (check-false (impersonator-of? into-list impersonated))
      (check chaperone-of? impersonated into-list)
      (check-false (chaperone-of? into-list impersonated)))))

(define/name (reducer/c domain-contract* range-contract*)
  (define domain-contract
    (coerce-contract enclosing-function-name domain-contract*))
  (define range-contract
    (coerce-contract enclosing-function-name range-contract*))
  (define contract-name
    (build-compound-type-name enclosing-function-name
                              domain-contract
                              range-contract))
  (define domain-projection (contract-late-neg-projection domain-contract))
  (define range-projection (contract-late-neg-projection range-contract))
  (define chaperone?
    (and (chaperone-contract? domain-contract)
         (chaperone-contract? range-contract)))
  (define (projection blame)
    (define domain-blame
      (blame-add-context blame "an element reduced by" #:swap? #t))
    (define range-blame (blame-add-context blame "the reduction result of"))
    (define late-neg-domain-guard (domain-projection domain-blame))
    (define late-neg-range-guard (range-projection range-blame))
    (λ (v missing-party)
      (assert-satisfies v reducer? blame #:missing-party missing-party)
      (define props
        (hash impersonator-prop:contracted the-contract
              impersonator-prop:blame (cons blame missing-party)))
      (define (domain-guard v) (late-neg-domain-guard v missing-party))
      (define (range-guard v) (late-neg-range-guard v missing-party))
      (reducer-impersonate v
                           #:domain-guard domain-guard
                           #:range-guard range-guard
                           #:chaperone? chaperone?
                           #:properties props)))
  (define the-contract
    ((if chaperone? make-chaperone-contract make-contract)
     #:name contract-name
     #:first-order reducer?
     #:late-neg-projection projection))
  the-contract)

(module+ test
  (test-case (name-string reducer/c)
    (test-case "should enforce the domain contract on sequence elements"
      (define/contract reducer (reducer/c number? any/c) into-list)
      (check-not-exn (λ () (reduce reducer 1 2 3)))
      (define (bad) (reduce reducer 1 2 'foo 3))
      (check-exn exn:fail:contract:blame? bad)
      (check-exn #rx"expected: number\\?" bad)
      (check-exn #rx"given: 'foo" bad)
      (check-exn #rx"an element reduced by" bad))

    (test-case "should enforce the range contract on reduction results"
      (define/contract reducer (reducer/c any/c integer?) into-sum)
      (check-not-exn (λ () (reduce reducer 1 2 3)))
      (define (bad) (reduce reducer 1 2 3.5))
      (check-exn exn:fail:contract:blame? bad)
      (check-exn #rx"promised: integer\\?" bad)
      (check-exn #rx"produced: 6\\.5" bad)
      (check-exn #rx"the reduction result of" bad))))

;@------------------------------------------------------------------------------
;; Non-core APIs

(define (reducer-map red #:domain [f values] #:range [g values])
  (define consumer (reducer-consumer red))
  (define finisher (reducer-finisher red))
  (define early-finisher (reducer-early-finisher red))
  (make-reducer #:starter (reducer-starter red)
                #:consumer (λ (s v) (consumer s (f v)))
                #:finisher (λ (s) (g (finisher s)))
                #:early-finisher (λ (s) (g (early-finisher s)))))

(define (reducer-filter red pred)
  (define consumer (reducer-consumer red))
  (define (filtering-consumer s v)
    (if (pred v)
        (consumer s v)
        (variant #:consume s)))
  (make-reducer #:starter (reducer-starter red)
                #:consumer filtering-consumer
                #:finisher (reducer-finisher red)
                #:early-finisher (reducer-early-finisher red)))

(define (reducer-limit red amount)
  (define-record-type limited-state (amount-left original-state))
  (define-wrapper-type original-result)
  (define-wrapper-type limited-result)

  (define original-consumer (reducer-consumer red))
  (define original-starter (reducer-starter red))
  (define original-finisher (reducer-finisher red))
  (define original-early-finisher (reducer-early-finisher red))

  (define/guard (start)
    (define original (original-starter))
    (guard (zero? amount) then
      (variant #:early-finish (limited-result (variant-value original))))
    (guard (variant-tagged-as? original '#:consume) else
      (variant #:early-finsih (original-result (variant-value original))))
    (define state
      (limited-state #:amount-left amount
                     #:original-state (variant-value original)))
    (variant #:consume state))

  (define/guard (consume state element)
    (define next-state
      (original-consumer (limited-state-original-state state) element))
    (define next-amount (sub1 (limited-state-amount-left state)))
    (guard (variant-tagged-as? next-state '#:early-finish) then
      (variant #:early-finish (original-result (variant-value next-state))))
    (guard (zero? next-amount) then
      (variant #:early-finish (limited-result (variant-value next-state))))
    (variant #:consume
             (limited-state #:amount-left next-amount
                            #:original-state (variant-value next-state))))

  (define (finish state)
    (original-finisher (limited-state-original-state state)))

  (define (early-finish state)
    (if (original-result? state)
        (original-early-finisher (original-result-value state))
        (original-finisher (limited-result-value state))))

  (make-reducer #:starter start
                #:consumer consume
                #:finisher finish
                #:early-finisher early-finish
                #:name 'limited))

(module+ test
  (test-case (name-string reducer-map)
    (define into-sum/string
      (reducer-map into-sum #:domain string->number #:range number->string))
    (check-equal? (reduce into-sum/string "1" "2" "3" "4" "5") "15"))

  (test-case (name-string reducer-filter)
    (define numbers-into-sum (reducer-filter into-sum number?))
    (check-equal? (reduce numbers-into-sum 1 'a 'b 2 3 4 'c 5 'd) 15))
  
  (test-case (name-string reducer-limit)
    (check-equal? (reduce-all (reducer-limit into-string 3) "hello") "hel")
    (check-equal? (reduce-all (reducer-limit into-string 9) "hello") "hello")
    (check-equal? (reduce-all (reducer-limit into-string 0) "hello") "")
    (check-equal? (reduce-all (reducer-limit (into-nth 2) 4) "hello")
                  (present #\l))
    (check-equal? (reduce-all (reducer-limit (into-nth 2) 1) "hello") absent)
    (check-equal? (reduce-all (reducer-limit (into-nth 2) 4) "he") absent)
    (check-equal? (reduce-all (reducer-limit (into-nth 2) 4) "hel")
                  (present #\l))
    (check-equal? (reduce-all (reducer-limit into-list 5) (in-naturals))
                  (list 0 1 2 3 4))))

(define-simple-macro
  (for/reducer/derived original reducer-expr:expr (for-clause ...)
    body ... tail-expr)
  (let ([reducer reducer-expr])
    (define starter (reducer-starter reducer))
    (define consumer (reducer-consumer reducer))
    (define finisher (reducer-finisher reducer))
    (define early-finisher (reducer-early-finisher reducer))
    (for/fold/derived original
      ([tagged-state (starter)]
       #:result (if (variant-tagged-as? tagged-state '#:consume)
                    (finisher (variant-value tagged-state))
                    (early-finisher (variant-value tagged-state))))
      (for-clause ... #:break (variant-tagged-as? tagged-state '#:early-finish))
      body ...
      (consumer (variant-value tagged-state) tail-expr))))

(define-simple-macro
  (for*/reducer/derived original reducer-expr:expr (for-clause ...)
    body ... tail-expr)
  (let ([reducer reducer-expr])
    (define starter (reducer-starter reducer))
    (define consumer (reducer-consumer reducer))
    (define finisher (reducer-finisher reducer))
    (define early-finisher (reducer-early-finisher reducer))
    (for*/fold/derived original
      ([tagged-state (starter)]
       #:result (if (variant-tagged-as? tagged-state '#:consume)
                    (finisher (variant-value tagged-state))
                    (early-finisher (variant-value tagged-state))))
      (for-clause ... #:break (variant-tagged-as? tagged-state '#:early-finish))
      body ...
      (consumer (variant-value tagged-state) tail-expr))))

(define-simple-macro (for/reducer reducer-expr:expr clauses body ... tail-expr)
  #:with original this-syntax
  (for/reducer/derived original reducer-expr clauses body ... tail-expr))

(define-simple-macro (for*/reducer reducer-expr:expr clauses body ... tail-expr)
  #:with original this-syntax
  (for*/reducer/derived original reducer-expr clauses body ... tail-expr))

(define into-list
  (reducer-map (make-fold-reducer (λ (lst v) (cons v lst)) (list))
               #:range reverse))

(module+ test
  (test-case (name-string for/reducer)
    (check-equal? (for/reducer into-list
                               ([n (in-naturals)]
                                [char (in-string "ab1c23d4ef")])
                    (if (char-alphabetic? char) n 'digit))
                  (list 0 1 'digit 3 'digit 'digit 6 'digit 8 9)))
  
  (test-case (name-string for*/reducer)
    (check-equal? (for*/reducer into-string
                                ([str (in-list (list "foo1" "bar2" "baz3"))]
                                 [char (in-string str)]
                                 #:when (char-alphabetic? char))
                    char)
                  "foobarbaz")))

(begin-for-syntax
  (define (make-reducer-based-for-comprehensions reducer-expr)
    (define (for-comprehension stx)
      (syntax-parse stx
        [(_ clauses body ... tail-expr)
         #:with original stx
         #:with reducer reducer-expr
         #'(for/reducer/derived original reducer clauses body ... tail-expr)]))
    (define (for*-comprehension stx)
      (syntax-parse stx
        [(_ clauses body ... tail-expr)
         #:with original stx
         #:with reducer reducer-expr
         #'(for*/reducer/derived original reducer clauses body ... tail-expr)]))
    (values for-comprehension for*-comprehension)))

(define into-string (reducer-map into-list #:range list->immutable-string))

(define (join-into-string sep
                          #:before-first [before-first ""]
                          #:before-last [before-last sep]
                          #:after-last [after-last ""])
  (define (join strs)
    (immutable-string-join strs sep
                           #:before-first before-first
                           #:before-last before-last
                           #:after-last after-last))
  (reducer-map into-list #:range join))

(define/name into-line
  (make-reducer
   #:starter (λ () (variant #:consume '()))
   #:consumer
   (λ (chars next-char)
     (if (equal? next-char #\newline)
         (variant #:early-finish chars)
         (variant #:consume (cons next-char chars))))
   #:finisher (λ (chars) (list->immutable-string (reverse chars)))
   #:early-finisher (λ (chars) (list->immutable-string (reverse chars)))
   #:name enclosing-variable-name))

(module+ test
  (test-case (name-string into-string)
    (check-equal? (reduce-all into-string "hello world") "hello world"))
  
  (test-case (name-string join-into-string)
    (check-equal? (reduce (join-into-string ", "
                                            #:before-first "["
                                            #:after-last "]")
                          "foo"
                          "bar"
                          "baz")
                  "[foo, bar, baz]"))
  
  (test-case (name-string into-line)
    (check-equal? (reduce-all into-line "hello \n world") "hello ")))

(define/name (into-nth n)
  (make-reducer
   #:starter (λ () (variant #:consume n))
   #:consumer
   (λ (n v)
     (if (zero? n)
         (variant #:early-finish (present v))
         (variant #:consume (sub1 n))))
   #:finisher (λ (_) absent)
   #:early-finisher values
   #:name enclosing-function-name))

;; TODO(https://github.com/jackfirth/rebellion/issues/187): this reducer should
;;   be named into-first but there's no reducer-rename operation yet.
(define into-first (into-nth 0))

(define/name into-last
  (make-fold-reducer (λ (_ v) (present v)) absent
                     #:name enclosing-variable-name))

(define/name (into-index-of v)
  (into-index-where (λ (elem) (equal? elem v)) #:name enclosing-function-name))

(define/name (into-index-where pred #:name [name enclosing-function-name])
  (make-reducer
   #:starter (λ () (variant #:consume 0))
   #:consumer
   (λ (index-counter elem)
     (if (pred elem)
         (variant #:early-finish index-counter)
         (variant #:consume (add1 index-counter))))
   #:finisher (λ (_) absent)
   #:early-finisher (λ (index-counter) (present index-counter))
   #:name name))

(define stateless-consume (variant #:consume #f))
(define stateless-early-finish (variant #:early-finish #f))

(define (stateless-starter) stateless-consume)

(define/name (into-any-match? pred)
  (make-reducer
   #:starter stateless-starter
   #:consumer (λ (_ v) (if (pred v) stateless-early-finish stateless-consume))
   #:finisher (λ (_) #f)
   #:early-finisher (λ (_) #t)
   #:name enclosing-function-name))

(define/name (into-all-match? pred)
  (make-reducer
   #:starter stateless-starter
   #:consumer (λ (_ v) (if (pred v) stateless-consume stateless-early-finish))
   #:finisher (λ (_) #t)
   #:early-finisher (λ (_) #f)
   #:name enclosing-function-name))

(define/name (into-none-match? pred)
  (make-reducer
   #:starter stateless-starter
   #:consumer (λ (_ v) (if (pred v) stateless-early-finish stateless-consume))
   #:finisher (λ (_) #t)
   #:early-finisher (λ (_) #f)
   #:name enclosing-function-name))

(define/name (into-for-each handler)
  (define consume-state (variant #:consume #f))
  (make-reducer
   #:starter (λ () consume-state)
   #:consumer
   (λ (_ element)
     (handler element)
     consume-state)
   #:finisher void
   #:early-finisher void
   #:name enclosing-function-name))

(module+ test
  (test-case (name-string into-nth)
    (check-equal? (reduce-all (into-nth 3) "magic") (present #\i))
    (check-equal? (reduce-all (into-nth 10) "magic") absent)
    (check-equal? (reduce-all (into-nth 0) (in-naturals)) (present 0))
    (check-equal? (reduce-all (into-nth 0) "") absent))
  
  (test-case (name-string into-first)
    (check-equal? (reduce-all into-first "horse") (present #\h))
    (check-equal? (reduce-all into-first "") absent))
  
  (test-case (name-string into-last)
    (check-equal? (reduce-all into-last "horse") (present #\e))
    (check-equal? (reduce-all into-last "") absent))
  
  (test-case (name-string into-index-of)
    (check-equal? (reduce-all (into-index-of #\f) "food") (present 0))
    (check-equal? (reduce-all (into-index-of #\f) "hoof") (present 3))
    (check-equal? (reduce-all (into-index-of #\f) "cat") absent))
  
  (test-case (name-string into-index-where)
    (check-equal? (reduce-all (into-index-where char-whitespace?) "hello world")
                  (present 5))
    (check-equal? (reduce-all (into-index-where char-numeric?) "goodbye world")
                  absent))

  (test-case (name-string into-any-match?)
    (define red (into-any-match? char-whitespace?))
    (check-true (reduce-all red "hello world"))
    (check-false (reduce-all red "hello"))
    (check-false (reduce-all red "")))

  (test-case (name-string into-all-match?)
    (define red (into-all-match? char-alphabetic?))
    (check-false (reduce-all red "hello world"))
    (check-true (reduce-all red "hello"))
    (check-true (reduce-all red "")))
  
  (test-case (name-string into-none-match?)
    (define red (into-none-match? char-whitespace?))
    (check-false (reduce-all red "hello world"))
    (check-true (reduce-all red "hello"))
    (check-true (reduce-all red "")))
  
  (test-case (name-string into-for-each)
    (define st (mutable-set))
    (define add-into-set! (into-for-each (λ (v) (set-add! st v))))
    (check-pred void? (reduce add-into-set! 1 2 3))
    (check-equal? st (mutable-set 1 2 3))))

(define-record-type candidate (element key))

(define (compute-candidate elem key-function)
  (candidate #:element elem #:key (key-function elem)))

(define/name (into-max [comparator real<=>] #:key [key-function values])
  (make-reducer
   #:starter (λ () (variant #:consume absent))
   #:consumer
   (λ (best-candidate elem)
     (guarded-block
       (define key (key-function elem))
       (guard (present? best-candidate) else
         (variant #:consume (present (candidate #:element elem #:key key))))
       (define best-key (candidate-key (present-value best-candidate)))
       (guard (equal? (compare comparator best-key key) lesser) else
         (variant #:consume best-candidate))
       (variant #:consume (present (candidate #:element elem #:key key)))))
   #:finisher (λ (best) (option-map best candidate-element))
   #:early-finisher values
   #:name enclosing-function-name))

(define (into-min [comparator real<=>] #:key [key-function values])
  (into-max (comparator-reverse comparator) #:key key-function))

(module+ test
  (test-case (name-string into-max)
    (check-equal? (reduce (into-max)) absent)
    (check-equal? (reduce (into-max) 2) (present 2))
    (check-equal? (reduce (into-max) 2 7 5 3) (present 7))
    
    (test-case "key-function"
      (define max (into-max #:key string-length))
      (check-equal? (reduce max "goodbye" "cruel" "world") (present "goodbye"))
      (check-equal? (reduce max "the" "quick" "brown" "fox") (present "quick")))
    
    (test-case "custom-comparator"
      (define max (into-max string<=>))
      (check-equal? (reduce max "a" "c" "b") (present "c"))
      (check-equal? (reduce max "abc" "aaa") (present "abc"))))
  
  (test-case (name-string into-min)
    (check-equal? (reduce (into-min) 4 8 2 16) (present 2))))
