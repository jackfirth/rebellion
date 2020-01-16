#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [comparator? predicate/c]
  [compare (-> comparator? any/c any/c comparison?)]
  [make-comparator
   (->* ((-> any/c any/c comparison?))
        (#:name (or/c interned-symbol? #f))
        comparator?)]
  [comparator-map
   (->* (comparator? (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        comparator?)]
  [comparator-reverse
   (-> comparator? comparator?)]
  [comparison? predicate/c]
  [lesser comparison?]
  [greater comparison?]
  [equivalent comparison?]
  [real<=> (comparator/c comparable-real?)]
  [string<=> (comparator/c immutable-string?)]
  [comparable-real? predicate/c]
  [comparator-impersonate
   (->* (comparator?)
        (#:operand-guard (or/c (-> any/c any/c) #f)
         #:properties impersonator-property-hash/c
         #:comparison-marks (and/c hash? immutable?)
         #:chaperone? boolean?)
        comparator?)]
  [comparator/c (-> contract? contract?)]))

(require racket/contract/combinator
         rebellion/base/immutable-string
         rebellion/base/symbol
         rebellion/private/contract-projection
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/private/strict-cond
         rebellion/type/reference
         rebellion/type/singleton)

(module+ test
  (require (submod "..")
           racket/contract/parametric
           racket/contract/region
           racket/function
           rackunit))

;@------------------------------------------------------------------------------

(define-reference-type comparator (function)
  #:constructor-name constructor:comparator)

(define-singleton-type lesser)
(define-singleton-type greater)
(define-singleton-type equivalent)

(define (comparison? v) (or (lesser? v) (greater? v) (equivalent? v)))

(define (make-comparator function* #:name [name #f])
  (define function
    (if (equal? (procedure-arity function*) 2)
        function*
        (procedure-reduce-arity function 2)))
  (constructor:comparator #:function function #:name name))

(define (compare comparator left right)
  ((comparator-function comparator) left right))

(define (comparator-map comparator mapper #:name [name #f])
  (define func (comparator-function comparator))
  (define (wrapped-func left right) (func (mapper left) (mapper right)))
  (make-comparator wrapped-func #:name name))

(define (comparator-reverse comparator)
  (define func (comparator-function comparator))
  (define (wrapped-func left right) (func right left))
  (make-comparator wrapped-func))

(define (comparable-real? v) (and (real? v) (not (equal? v +nan.0))))

(define/name real<=>
  (make-comparator
   (λ (x y)
     (strict-cond [(< x y) lesser] [(= x y) equivalent] [(> x y) greater]))
   #:name enclosing-variable-name))

(define/name string<=>
  (make-comparator
   (λ (s1 s2)
     (cond [(immutable-string<? s1 s2) lesser]
           [(equal? s1 s2) equivalent]
           [else greater]))
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

  (test-case (name-string string<=>)
    (check-equal? (compare string<=> "apple" "banana") lesser)
    (check-equal? (compare string<=> "apple" "aardvark") greater)
    (check-equal? (compare string<=> "apple" "apple") equivalent))
  
  (test-case (name-string comparator-reverse)
    (define reversed (comparator-reverse real<=>))
    (check-equal? (compare reversed 1 2) greater)
    (check-equal? (compare reversed 2 1) lesser)
    (check-equal? (compare reversed 1 1) equivalent)))

;@------------------------------------------------------------------------------
;; Contracts

(define ((comparator-function-guard argument-guard) left right)
  (values (argument-guard left) (argument-guard right)))

(define (comparator-impersonate comparator
                                #:operand-guard [guard #f]
                                #:properties [properties (hash)]
                                #:comparison-marks [marks (hash)]
                                #:chaperone? [chaperone? (not guard)])
  (define function (comparator-function comparator))
  (define impersonated-function
    (function-impersonate function
                          #:guard (and guard (comparator-function-guard guard))
                          #:application-marks marks
                          #:chaperone? chaperone?))
  (define impersonated-without-props
    (make-comparator impersonated-function #:name (object-name comparator)))
  (reference-impersonate impersonated-without-props descriptor:comparator
                         #:properties properties))

(module+ test
  (test-case (name-string comparator-impersonate)
    (define (ensure-integer v)
      (if (integer? v) v (raise-argument-error 'integer<=> "integer?" v)))
    (check-equal? (ensure-integer 5) 5)
    (define chaperoned
      (comparator-impersonate real<=>
                              #:operand-guard ensure-integer
                              #:chaperone? #t))
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
    (define operand-blame (blame-add-context blame "an operand of" #:swap? #t))
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
