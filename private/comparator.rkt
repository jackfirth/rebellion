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
  [string<=> comparator?]))

(require rebellion/base/immutable-string
         rebellion/base/symbol
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

(define (make-comparator function #:name [name #f])
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

(define real<=>
  (make-comparator
   (λ (x y) (cond [(< x y) lesser] [(= x y) equivalent] [else greater]))
   #:name 'real<=>))

(define string<=>
  (make-comparator
   (λ (s1 s2)
     (cond [(immutable-string<? s1 s2) lesser]
           [(equal? s1 s2) equivalent]
           [else greater]))))

(module+ test
  (test-case "real<=>"
    (check-equal? (compare real<=> 4 5.2) lesser)
    (check-equal? (compare real<=> 0 -7) greater)
    (check-equal? (compare real<=> 3 3) equivalent))
  (test-case "string<=>"
    (check-equal? (compare string<=> "apple" "banana") lesser)
    (check-equal? (compare string<=> "apple" "aardvark") greater)
    (check-equal? (compare string<=> "apple" "apple") equivalent))
  (test-case "comparator-reverse"
    (define reversed (comparator-reverse real<=>))
    (check-equal? (compare reversed 1 2) greater)
    (check-equal? (compare reversed 2 1) lesser)
    (check-equal? (compare reversed 1 1) equivalent)))
