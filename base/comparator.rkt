#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [comparator? predicate/c]
  [compare (-> comparator? any/c any/c comparison?)]
  [make-comparator
   (->* ((-> any/c any/c comparison?)) (#:name (or/c interned-symbol? #false)) comparator?)]
  [comparator-map
   (->* (comparator? (-> any/c any/c)) (#:name (or/c interned-symbol? #false)) comparator?)]
  [comparator-reverse (-> comparator? comparator?)]
  [comparator-chain (-> comparator? comparator? ... comparator?)]
  [comparator-of-constants (-> any/c ... comparator?)]
  [comparator-min (-> comparator? any/c any/c ... any/c)]
  [comparator-max (-> comparator? any/c any/c ... any/c)]
  [comparison? predicate/c]
  [lesser comparison?]
  [greater comparison?]
  [equivalent comparison?]
  [real<=> (comparator/c comparable-real?)]
  [natural<=> (comparator/c natural?)]
  [string<=> (comparator/c immutable-string?)]
  [char<=> (comparator/c char?)]
  [symbol<=> (comparator/c symbol?)]
  [interned-symbol<=> (comparator/c interned-symbol?)]
  [comparable-real? predicate/c]
  [comparator-impersonate
   (->* (comparator?)
        (#:operand-guard (or/c (-> any/c any/c) #false)
         #:properties impersonator-property-hash/c
         #:comparison-marks (and/c hash? immutable?)
         #:chaperone? boolean?)
        comparator?)]
  [comparator/c (-> contract? contract?)]))

(require racket/contract/combinator
         racket/list
         racket/math
         racket/set
         rebellion/base/immutable-string
         rebellion/base/symbol
         rebellion/private/contract-projection
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/private/strict-cond
         rebellion/type/object
         rebellion/type/singleton)

(module+ test
  (require (submod "..")
           racket/contract/parametric
           racket/contract/region
           racket/function
           rackunit))

;@------------------------------------------------------------------------------

(define-object-type comparator (function)
  #:constructor-name constructor:comparator)

(define-singleton-type lesser)
(define-singleton-type greater)
(define-singleton-type equivalent)

(define (comparison? v) (or (lesser? v) (greater? v) (equivalent? v)))

(define (make-comparator function* #:name [name #false])
  (define function
    (if (equal? (procedure-arity function*) 2)
        function*
        (procedure-reduce-arity function* 2)))
  (constructor:comparator #:function function #:name name))

(define (compare comparator left right)
  ((comparator-function comparator) left right))

(define (comparator-map comparator mapper #:name [name #false])
  (define func (comparator-function comparator))
  (define (wrapped-func left right) (func (mapper left) (mapper right)))
  (make-comparator wrapped-func #:name name))

(define (comparator-reverse comparator)
  (define func (comparator-function comparator))
  (define (wrapped-func left right) (func right left))
  (make-comparator wrapped-func))

(define (comparator-chain #:name [name 'chained] . comparators)
  (make-comparator
   (λ (x y)
     (define chain-result
       (for/or ([comparator (in-list comparators)])
         (define result (compare comparator x y))
         (and (not (equal? result equivalent)) result)))
     (or chain-result equivalent))
   #:name name))

(define (has-duplicates? lst)
  (not (equal? (set-count (list->set lst)) (length lst))))

(define/name (comparator-of-constants . ascending-constants)
  (when (has-duplicates? ascending-constants)
    (define duplicate (check-duplicates ascending-constants))
    (raise-arguments-error
     enclosing-function-name
     "contract violation;\n constants must be unique to prevent ambiguity"
     "constants" ascending-constants
     "duplicate" duplicate))
  (make-comparator
   (λ (x y)
     (define i (index-of ascending-constants x))
     (define j (index-of ascending-constants y))
     (unless i
       (raise-argument-error enclosing-function-name (format "one of ~v" ascending-constants) x))
     (unless j
       (raise-argument-error enclosing-function-name (format "one of ~v" ascending-constants) y))
     (compare real<=> i j))
   #:name enclosing-function-name))


(define (comparator-min comparator v . vs)
  (for/fold ([min-so-far v])
            ([v (in-list vs)])
    (if (equal? (compare comparator v min-so-far) lesser)
        v
        min-so-far)))


(define (comparator-max comparator v . vs)
  (for/fold ([max-so-far v])
            ([v (in-list vs)])
    (if (equal? (compare comparator v max-so-far) greater)
        v
        max-so-far)))


(define (comparable-real? v) (and (real? v) (not (equal? v +nan.0))))

(define/name real<=>
  (make-comparator
   (λ (x y)
     (strict-cond [(< x y) lesser] [(= x y) equivalent] [(> x y) greater]))
   #:name enclosing-variable-name))

(define/name natural<=>
  (make-comparator
   (λ (x y)
     (define difference (- x y))
     (strict-cond [(zero? difference) equivalent] [(positive? difference) greater] [else lesser]))
   #:name enclosing-variable-name))

(define/name string<=>
  (make-comparator
   (λ (s1 s2)
     (cond [(immutable-string<? s1 s2) lesser]
           [(equal? s1 s2) equivalent]
           [else greater]))
   #:name enclosing-variable-name))

(define/name char<=>
  (make-comparator
   (λ (c1 c2) (cond [(char<? c1 c2) lesser] [(equal? c1 c2) equivalent] [else greater]))
   #:name enclosing-variable-name))

(define/name symbol<=>
  (make-comparator
   (λ (s1 s2)
     (cond
       [(eq? s1 s2) equivalent]
       [(symbol<? s1 s2) lesser]
       [(symbol<? s2 s1) greater]
       [else equivalent]))
   #:name enclosing-variable-name))

(define/name interned-symbol<=>
  (make-comparator
   (λ (s1 s2) (cond [(eq? s1 s2) equivalent] [(symbol<? s1 s2) lesser] [else greater]))
   #:name enclosing-variable-name))

(module+ test
  (test-case (name-string real<=>)
    (check-equal? (compare real<=> 4 5.2) lesser)
    (check-equal? (compare real<=> 0 -7) greater)
    (check-equal? (compare real<=> 3 3) equivalent)

    (test-case "should ignore exactness"
      (check-equal? (compare real<=> 5 5.0) equivalent)
      (check-equal? (compare real<=> 5.0 5) equivalent))

    (test-case "should treat all variants of zero as equivalent"
      (check-equal? (compare real<=> 0 0.0) equivalent)
      (check-equal? (compare real<=> 0 -0.0) equivalent)
      (check-equal? (compare real<=> 0.0 -0.0) equivalent))

    (test-case "should not allow comparing values to the not-a-number constants"
      (check-exn exn:fail:contract:blame? (λ () (compare real<=> 3 +nan.0)))
      (check-exn exn:fail:contract:blame? (λ () (compare real<=> +nan.0 3)))
      (check-exn exn:fail:contract:blame? (λ () (compare real<=> 3 +nan.f)))
      (check-exn exn:fail:contract:blame? (λ () (compare real<=> +nan.f 3))))

    (test-case "should allow comparisons to infinities"
      (check-equal? (compare real<=> 4 +inf.0) lesser)
      (check-equal? (compare real<=> 4 +inf.f) lesser)
      (check-equal? (compare real<=> 4 -inf.0) greater)
      (check-equal? (compare real<=> 4 -inf.f) greater)
      (check-equal? (compare real<=> -inf.0 +inf.0) lesser)
      (check-equal? (compare real<=> -inf.f +inf.f) lesser))

    (test-case "should treat same-signed infinities as equivalent"
      (check-equal? (compare real<=> +inf.0 +inf.f) equivalent)
      (check-equal? (compare real<=> +inf.f +inf.0) equivalent)
      (check-equal? (compare real<=> -inf.0 -inf.f) equivalent)
      (check-equal? (compare real<=> -inf.f -inf.0) equivalent)))

  (test-case (name-string natural<=>)
    (check-equal? (compare natural<=> 4 10) lesser)
    (check-equal? (compare natural<=> 4 0) greater)
    (check-equal? (compare natural<=> 4 4) equivalent))

  (test-case (name-string char<=>)
    (check-equal? (compare char<=> #\e #\s) lesser)
    (check-equal? (compare char<=> #\e #\a) greater)
    (check-equal? (compare char<=> #\e #\e) equivalent))

  (test-case (name-string string<=>)
    (check-equal? (compare string<=> "apple" "banana") lesser)
    (check-equal? (compare string<=> "apple" "aardvark") greater)
    (check-equal? (compare string<=> "apple" "apple") equivalent))

  (test-case (name-string symbol<=>)
    (check-equal? (compare symbol<=> 'apple 'banana) lesser)
    (check-equal? (compare symbol<=> 'apple 'aardvark) greater)
    (check-equal? (compare symbol<=> 'apple 'apple) equivalent)
    (check-equal? (compare symbol<=> 'apple (string->unreadable-symbol "apple")) equivalent)
    (check-equal? (compare symbol<=> 'apple (string->uninterned-symbol "apple")) equivalent))

  (test-case (name-string interned-symbol<=>)
    (check-equal? (compare interned-symbol<=> 'apple 'banana) lesser)
    (check-equal? (compare interned-symbol<=> 'apple 'aardvark) greater)
    (check-equal? (compare interned-symbol<=> 'apple 'apple) equivalent)
    (check-exn
     exn:fail:contract?
     (λ () (compare interned-symbol<=> 'apple (string->unreadable-symbol "apple"))))
    (check-exn
     exn:fail:contract?
     (λ () (compare interned-symbol<=> 'apple (gensym 'apple)))))
  
  (test-case (name-string comparator-reverse)
    (define reversed (comparator-reverse real<=>))
    (check-equal? (compare reversed 1 2) greater)
    (check-equal? (compare reversed 2 1) lesser)
    (check-equal? (compare reversed 1 1) equivalent))

  (test-case (name-string comparator-chain)
    (struct gemstone (type weight) #:transparent)
    (define gemstone<=>
      (comparator-chain
       (comparator-map (comparator-of-constants 'opal 'ruby) gemstone-type)
       (comparator-map real<=> gemstone-weight)))
    (check-equal? (compare gemstone<=> (gemstone 'opal 5) (gemstone 'opal 5)) equivalent)
    (check-equal? (compare gemstone<=> (gemstone 'opal 5) (gemstone 'opal 3)) greater)
    (check-equal? (compare gemstone<=> (gemstone 'opal 5) (gemstone 'opal 8)) lesser)
    (check-equal? (compare gemstone<=> (gemstone 'opal 5) (gemstone 'ruby 5)) lesser)
    (check-equal? (compare gemstone<=> (gemstone 'opal 5) (gemstone 'ruby 3)) lesser)
    (check-equal? (compare gemstone<=> (gemstone 'opal 5) (gemstone 'ruby 8)) lesser))

  (test-case (name-string comparator-of-constants)
    (define size<=> (comparator-of-constants 'small 'medium 'large))
    (check-equal? (compare size<=> 'small 'medium) lesser)
    (check-equal? (compare size<=> 'small 'large) lesser)
    (check-equal? (compare size<=> 'medium 'large) lesser)
    (check-equal? (compare size<=> 'medium 'small) greater)
    (check-equal? (compare size<=> 'large 'small) greater)
    (check-equal? (compare size<=> 'large 'medium) greater)
    (check-equal? (compare size<=> 'medium 'medium) equivalent)
    (check-exn exn:fail:contract? (λ () (compare size<=> 'small 'short)))
    (check-exn #rx"expected: one of '\\(small medium large\\)" (λ () (compare size<=> 'small 'short)))
    (check-exn #rx"given: 'short" (λ () (compare size<=> 'small 'short)))
    (check-exn exn:fail:contract? (λ () (comparator-of-constants 'small 'medium 'small)))
    (check-exn #rx"'small" (λ () (comparator-of-constants 'small 'medium 'small)))
    (check-exn
     #rx"'\\(small medium small\\)" (λ () (comparator-of-constants 'small 'medium 'small)))
    (check-exn exn:fail:contract? (λ () (comparator-of-constants #false #false))))

  (test-case (name-string comparator-min)
    (check-equal? (comparator-min real<=> 1 99 99) 1)
    (check-equal? (comparator-min real<=> 99 1 99) 1)
    (check-equal? (comparator-min real<=> 99 99 1) 1))

  (test-case (name-string comparator-max)
    (check-equal? (comparator-max real<=> 99 1 1) 99)
    (check-equal? (comparator-max real<=> 1 99 1) 99)
    (check-equal? (comparator-max real<=> 1 1 99) 99)))

;@------------------------------------------------------------------------------
;; Contracts

(define ((comparator-function-guard argument-guard) left right)
  (values (argument-guard left) (argument-guard right)))

(define (comparator-impersonate comparator
                                #:operand-guard [guard #false]
                                #:properties [properties (hash)]
                                #:comparison-marks [marks (hash)]
                                #:chaperone? [chaperone? (not guard)])
  (define function (comparator-function comparator))
  (define impersonated-function
    (function-impersonate
     function
     #:arguments-guard (and guard (comparator-function-guard guard))
     #:application-marks marks
     #:chaperone? chaperone?))
  (define impersonated-without-props
    (make-comparator impersonated-function #:name (object-name comparator)))
  (object-impersonate impersonated-without-props descriptor:comparator
                      #:properties properties))

(module+ test
  (test-case (name-string comparator-impersonate)
    (define (ensure-integer v)
      (if (integer? v) v (raise-argument-error 'integer<=> "integer?" v)))
    (check-equal? (ensure-integer 5) 5)
    (define chaperoned
      (comparator-impersonate real<=>
                              #:operand-guard ensure-integer
                              #:chaperone? #true))
    (check-equal? (compare chaperoned 5 8) (compare real<=> 5 8))
    (check-exn exn:fail:contract? (λ () (compare chaperoned 5 7.5)))
    (check-equal? chaperoned real<=>)
    (check-true (chaperone-of? chaperoned real<=>))
    (check-true (impersonator-of? chaperoned real<=>))
    (check-false (chaperone-of? real<=> chaperoned))
    (check-false (impersonator-of? real<=> chaperoned))))

(define/name (comparator/c operand-contract*)
  (define operand-contract
    (coerce-contract enclosing-function-name operand-contract*))
  (define contract-name
    (build-compound-type-name enclosing-function-name operand-contract))
  (define operand-projection (contract-late-neg-projection operand-contract))
  (define chaperone? (chaperone-contract? operand-contract))
  (define (projection blame)
    (define operand-blame
      (blame-add-context blame "an operand of" #:swap? #true))
    (define late-neg-operand-guard (operand-projection operand-blame))
    (λ (v missing-party)
      (assert-satisfies v comparator? blame #:missing-party missing-party)
      (define props
        (hash impersonator-prop:contracted the-contract
              impersonator-prop:blame (cons blame missing-party)))
      (define (operand-guard v) (late-neg-operand-guard v missing-party))
      (comparator-impersonate v
                              #:operand-guard operand-guard
                              #:chaperone? chaperone?
                              #:properties props)))
  (define the-contract
    ((if chaperone? make-chaperone-contract make-contract)
     #:name contract-name
     #:first-order comparator?
     #:late-neg-projection projection))
  the-contract)

(module+ test
  (test-case (name-string comparator/c)

    (define digit<=>
      (make-comparator
       (λ (x y)
         (strict-cond
           [(equal? x y) equivalent]
           [(< x y) lesser]
           [(> x y) greater]))
       #:name (name digit<=>)))

    (define digit/c (integer-in 0 9))

    (test-case "should make chaperones for non-impersonator operand contracts"
      (check-pred chaperone-contract? (comparator/c any/c))
      (check-pred chaperone-contract? (comparator/c string?))
      (check-pred chaperone-contract? (comparator/c (-> any/c any/c)))
      (check-pred (negate chaperone-contract?) (comparator/c (new-∀/c))))

    (test-case "should only enforce comparator? predicate in first order checks"
      (check-exn exn:fail:contract:blame?
                 (λ () (invariant-assertion (comparator/c any/c) 42)))
      (check-not-exn (λ () (invariant-assertion (comparator/c any/c) digit<=>)))
      (check-not-exn
       (λ () (invariant-assertion (comparator/c digit/c) digit<=>)))
      (check-not-exn
       (λ () (invariant-assertion (comparator/c string?) digit<=>))))
    
    (test-case "should enforce flat operand contracts"
      (define/contract contracted (comparator/c digit/c) digit<=>)
      (check-not-exn (λ () (compare contracted 5 5)))
      (check-exn exn:fail:contract:blame? (λ () (compare contracted 5 10)))
      (check-exn exn:fail:contract:blame? (λ () (compare contracted -1 5)))
      (check-exn exn:fail:contract:blame? (λ () (compare contracted 5 "foo")))
      (check-exn exn:fail:contract:blame? (λ () (compare contracted "foo" 5))))

    (test-case "should add contract system impersonator properties"
      (define the-contract (comparator/c digit/c))
      (define/contract contracted the-contract digit<=>)
      (check-pred has-contract? contracted)
      (check-equal? (value-contract contracted) the-contract)
      (check-pred has-blame? contracted))))
