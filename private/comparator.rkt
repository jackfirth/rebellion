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
  [real<=> comparator?]
  [string<=> comparator?]
  [comparator-impersonate
   (->* (comparator?)
        (#:operand-guard (or/c (-> any/c any/c) #f)
         #:properties impersonator-property-hash/c
         #:comparison-marks (and/c hash? immutable?)
         #:chaperone? boolean?)
        comparator?)]))

(require rebellion/base/immutable-string
         rebellion/base/symbol
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/type/reference
         rebellion/type/singleton)

(module+ test
  (require (submod "..")
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

(define/name real<=>
  (make-comparator
   (λ (x y) (cond [(< x y) lesser] [(= x y) equivalent] [else greater]))
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
    (check-equal? (compare real<=> 3 3) equivalent))
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
