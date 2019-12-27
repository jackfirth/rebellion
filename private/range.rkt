#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [range? predicate/c]

  [range
   (->i #:chaperone
        ([lower-bound (or/c range-bound? unbounded?)]
         [upper-bound (or/c range-bound? unbounded?)])
        (#:comparator [cmp comparator?])

        #:pre/name (lower-bound upper-bound cmp)
        "lower endpoint must be less than or equal to upper endpoint"
        (cond
          [(unbounded? lower-bound) #t]
          [(unbounded? upper-bound) #t]
          [else
           (define lower (range-bound-endpoint lower-bound))
           (define upper (range-bound-endpoint upper-bound))
           (not (equal? (compare (default-real<=> cmp) lower upper) greater))])

        #:pre/name (lower-bound upper-bound)
        "equal endpoints cannot both be exclusive"
        (not (and (exclusive-bound? lower-bound)
                  (exclusive-bound? upper-bound)
                  (equal? (exclusive-bound-endpoint lower-bound)
                          (exclusive-bound-endpoint upper-bound))))

        [_ range?])]
  
  [closed-range
   (->i #:chaperone
        ([lower any/c]
         [upper any/c])
        (#:comparator [cmp comparator?])

        #:pre/name (lower upper cmp)
        "lower endpoint must be less than or equal to upper endpoint"
        (not (equal? (compare (default-real<=> cmp) lower upper) greater))

        [_ range?])]

  [open-range
   (->i #:chaperone
        ([lower any/c]
         [upper any/c])
        (#:comparator [cmp comparator?])

        #:pre/name (lower upper cmp)
        "lower endpoint must be less than upper endpoint"
        (equal? (compare (default-real<=> cmp) lower upper) lesser)

        [_ range?])]

  [closed-open-range
   (->i #:chaperone
        ([lower any/c]
         [upper any/c])
        (#:comparator [cmp comparator?])

        #:pre/name (lower upper cmp)
        "lower endpoint must be less than or equal to upper endpoint"
        (not (equal? (compare (default-real<=> cmp) lower upper) greater))

        [_ range?])]

  [open-closed-range
   (->i #:chaperone
        ([lower any/c]
         [upper any/c])
        (#:comparator [cmp comparator?])

        #:pre/name (lower upper cmp)
        "lower endpoint must be less than or equal to upper endpoint"
        (not (equal? (compare (default-real<=> cmp) lower upper) greater))

        [_ range?])]

  [at-least single-endpoint-range-constructor/c]
  [at-most single-endpoint-range-constructor/c]
  [less-than single-endpoint-range-constructor/c]
  [greater-than single-endpoint-range-constructor/c]
  [singleton-range single-endpoint-range-constructor/c]
  [range-contains? (-> range? any/c boolean?)]
  [range-lower-bound (-> range? (or/c range-bound? unbounded?))]
  [range-upper-bound (-> range? (or/c range-bound? unbounded?))]
  [range-comparator (-> range? comparator?)]
  [unbounded? predicate/c]
  [unbounded unbounded?]
  [range-bound-type? predicate/c]
  [inclusive range-bound-type?]
  [exclusive range-bound-type?]
  [range-bound? predicate/c]
  [range-bound-endpoint (-> range-bound? any/c)]
  [range-bound-type (-> range-bound? range-bound-type?)]
  [inclusive-bound (-> any/c range-bound?)]
  [exclusive-bound (-> any/c range-bound?)]))

(require rebellion/base/comparator
         rebellion/type/singleton
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------
;; Data model

(define-tuple-type range (lower-bound upper-bound comparator)
  #:constructor-name constructor:range)

(define-singleton-type unbounded)

(define-singleton-type inclusive)
(define-singleton-type exclusive)

(define-tuple-type inclusive-bound (endpoint))
(define-tuple-type exclusive-bound (endpoint))

(define (range-bound? v)
  (or (inclusive-bound? v) (exclusive-bound? v)))

(define (range-bound-type? v)
  (or (inclusive? v) (exclusive? v)))

(define (range-bound-type bound)
  (if (inclusive-bound? bound) inclusive exclusive))

(define (range-bound-endpoint bound)
  (if (inclusive-bound? bound)
      (inclusive-bound-endpoint bound)
      (exclusive-bound-endpoint bound)))

(define (range lower-bound upper-bound #:comparator [comparator real<=>])
  (constructor:range lower-bound upper-bound comparator))

(define (range-contains? rng v)
  (define lower (range-lower-bound rng))
  (define upper (range-upper-bound rng))
  (define cmp (range-comparator rng))
  (define within-lower-bound?
    (cond
      [(unbounded? lower) #t]
      [else
       (define result (compare cmp (range-bound-endpoint lower) v))
       (if (exclusive-bound? lower)
           (equal? result lesser)
           (not (equal? result greater)))]))
  (define within-upper-bound?
    (cond
      [(unbounded? upper) #t]
      [else
       (define result (compare cmp v (range-bound-endpoint upper)))
       (if (exclusive-bound? upper)
           (equal? result lesser)
           (not (equal? result greater)))]))
  (and within-lower-bound? within-upper-bound?))

;@------------------------------------------------------------------------------
;; Smart constructors

(define ((range-factory lower-bound-maker upper-bound-maker)
         lower-endpoint upper-endpoint #:comparator [comparator real<=>])
  (range (lower-bound-maker lower-endpoint) (upper-bound-maker upper-endpoint)
         #:comparator comparator))

(define closed-range (range-factory inclusive-bound inclusive-bound))
(define open-range (range-factory exclusive-bound exclusive-bound))
(define closed-open-range (range-factory inclusive-bound exclusive-bound))
(define open-closed-range (range-factory exclusive-bound inclusive-bound))

(define (at-least endpoint #:comparator [comparator real<=>])
  (range (inclusive-bound endpoint) unbounded #:comparator comparator))

(define (at-most endpoint #:comparator [comparator real<=>])
  (range unbounded (inclusive-bound endpoint) #:comparator comparator))

(define (greater-than endpoint #:comparator [comparator real<=>])
  (range (exclusive-bound endpoint) unbounded #:comparator comparator))

(define (less-than endpoint #:comparator [comparator real<=>])
  (range unbounded (exclusive-bound endpoint) #:comparator comparator))

(define (singleton-range endpoint #:comparator [comparator real<=>])
  (define bound (inclusive-bound endpoint))
  (range bound bound #:comparator comparator))

(module+ test
  (test-case (name-string range)
    (check-exn exn:fail:contract?
               (λ () (range (inclusive-bound 5) (inclusive-bound 2))))
    (check-exn exn:fail:contract?
               (λ () (range (inclusive-bound 5) (exclusive-bound 2))))
    (check-exn exn:fail:contract?
               (λ () (range (exclusive-bound 5) (inclusive-bound 2))))
    (check-exn exn:fail:contract?
               (λ () (range (exclusive-bound 5) (exclusive-bound 2))))
    (check-exn exn:fail:contract?
               (λ () (range (exclusive-bound 3) (exclusive-bound 3)))))
  
  (test-case (name-string closed-range)
    (define rng (closed-range 2 4))
    (check-false (range-contains? rng 1))
    (check-true (range-contains? rng 2))
    (check-true (range-contains? rng 3))
    (check-true (range-contains? rng 4))
    (check-false (range-contains? rng 5))
    (check-exn exn:fail:contract? (λ () (closed-range 4 2)))
    (check-true (range-contains? (closed-range 3 3) 3)))

  (test-case (name-string open-range)
    (define rng (open-range 2 4))
    (check-false (range-contains? rng 1))
    (check-false (range-contains? rng 2))
    (check-true (range-contains? rng 3))
    (check-false (range-contains? rng 4))
    (check-false (range-contains? rng 5))
    (check-exn exn:fail:contract? (λ () (open-range 4 2)))
    (check-exn exn:fail:contract? (λ () (open-range 3 3))))

  (test-case (name-string closed-open-range)
    (define rng (closed-open-range 2 4))
    (check-false (range-contains? rng 1))
    (check-true (range-contains? rng 2))
    (check-true (range-contains? rng 3))
    (check-false (range-contains? rng 4))
    (check-false (range-contains? rng 5))
    (check-exn exn:fail:contract? (λ () (closed-open-range 4 2)))
    (check-false (range-contains? (closed-open-range 3 3) 3)))

  (test-case (name-string open-closed-range)
    (define rng (open-closed-range 2 4))
    (check-false (range-contains? rng 1))
    (check-false (range-contains? rng 2))
    (check-true (range-contains? rng 3))
    (check-true (range-contains? rng 4))
    (check-false (range-contains? rng 5))
    (check-exn exn:fail:contract? (λ () (open-closed-range 4 2)))
    (check-false (range-contains? (open-closed-range 3 3) 3)))

  (test-case (name-string at-least)
    (define rng (at-least 2))
    (check-false (range-contains? rng 1))
    (check-true (range-contains? rng 2))
    (check-true (range-contains? rng 3)))

  (test-case (name-string at-most)
    (define rng (at-most 2))
    (check-true (range-contains? rng 1))
    (check-true (range-contains? rng 2))
    (check-false (range-contains? rng 3)))

  (test-case (name-string greater-than)
    (define rng (greater-than 2))
    (check-false (range-contains? rng 1))
    (check-false (range-contains? rng 2))
    (check-true (range-contains? rng 3)))

  (test-case (name-string less-than)
    (define rng (less-than 2))
    (check-true (range-contains? rng 1))
    (check-false (range-contains? rng 2))
    (check-false (range-contains? rng 3)))

  (test-case (name-string singleton-range)
    (define rng (singleton-range 42))
    (check-true (range-contains? rng 42))
    (check-false (range-contains? rng 41))
    (check-false (range-contains? rng 43))))

;@------------------------------------------------------------------------------
;; Contract helpers

(define (default-real<=> v) (if (unsupplied-arg? v) real<=> v))

(define single-endpoint-range-constructor/c
  (->* (any/c) (#:comparator comparator?) range?))
