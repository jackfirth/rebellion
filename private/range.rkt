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
        (strict-cond
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

  [at-least-range single-endpoint-range-constructor/c]
  [at-most-range single-endpoint-range-constructor/c]
  [less-than-range single-endpoint-range-constructor/c]
  [greater-than-range single-endpoint-range-constructor/c]
  [singleton-range single-endpoint-range-constructor/c]
  [unbounded-range (->* () (#:comparator comparator?) range?)]
  [unbounded-above-range (->* (range-bound?) (#:comparator comparator?) range?)]
  [unbounded-below-range (->* (range-bound?) (#:comparator comparator?) range?)]
  [range-contains? (-> range? any/c boolean?)]

  [range-encloses?
   (->i #:chaperone
        ([range range?] [other-range range?])

        #:pre/name (range other-range)
        "both ranges must use the same comparator"
        (equal? (range-comparator range) (range-comparator other-range))

        [_ boolean?])]

  [range-connected?
   (->i #:chaperone
        ([range1 range?] [range2 range?])

        #:pre/name (range1 range2)
        "both ranges must use the same comparator"
        (equal? (range-comparator range1) (range-comparator range2))

        [_ boolean?])]

  [range-span
   (->i #:chaperone
        ([range1 range?] [range2 range?])

        #:pre/name (range1 range2)
        "both ranges must use the same comparator"
        (equal? (range-comparator range1) (range-comparator range2))

        [_ range?])]

  [range-lower-bound (-> range? (or/c range-bound? unbounded?))]
  [range-upper-bound (-> range? (or/c range-bound? unbounded?))]
  [range-comparator (-> range? comparator?)]
  [unbounded? predicate/c]
  [unbounded unbounded?]
  [range-bound-type? predicate/c]
  [inclusive range-bound-type?]
  [exclusive range-bound-type?]
  [range-bound? predicate/c]
  [range-bound (-> any/c range-bound-type? range-bound?)]
  [range-bound-endpoint (-> range-bound? any/c)]
  [range-bound-type (-> range-bound? range-bound-type?)]
  [inclusive-bound (-> any/c range-bound?)]
  [exclusive-bound (-> any/c range-bound?)]))

(require racket/bool
         rebellion/base/comparator
         rebellion/private/static-name
         rebellion/private/strict-cond
         rebellion/type/enum
         rebellion/type/singleton
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------
;; Data model

(define-tuple-type range (lower-bound upper-bound comparator)
  #:constructor-name constructor:range)

(define-singleton-type unbounded)

(define-enum-type range-bound-type (inclusive exclusive))

(define-tuple-type inclusive-bound (endpoint))
(define-tuple-type exclusive-bound (endpoint))

(define (range-bound? v)
  (or (inclusive-bound? v) (exclusive-bound? v)))

(define (range-bound endpoint type)
  (strict-cond
    [(equal? type inclusive) (inclusive-bound endpoint)]
    [else (exclusive-bound endpoint)]))

(define (range-bound-type bound)
  (if (inclusive-bound? bound) inclusive exclusive))

(define (range-bound-endpoint bound)
  (if (inclusive-bound? bound)
      (inclusive-bound-endpoint bound)
      (exclusive-bound-endpoint bound)))

(define-tuple-type upper-cut (value))
(define-tuple-type middle-cut (value))
(define-tuple-type lower-cut (value))
(define-singleton-type top-cut)
(define-singleton-type bottom-cut)

(define (intermediate-cut-value cut)
  (strict-cond
    [(upper-cut? cut) (upper-cut-value cut)]
    [(middle-cut? cut) (middle-cut-value cut)]
    [(lower-cut? cut) (lower-cut-value cut)]))

(define (range-lower-cut range)
  (define bound (range-lower-bound range))
  (strict-cond
    [(unbounded? bound) bottom-cut]
    [(inclusive-bound? bound) (lower-cut (range-bound-endpoint bound))]
    [else (upper-cut (range-bound-endpoint bound))]))

(define (range-upper-cut range)
  (define bound (range-upper-bound range))
  (strict-cond
    [(unbounded? bound) top-cut]
    [(inclusive-bound? bound) (upper-cut (range-bound-endpoint bound))]
    [else (lower-cut (range-bound-endpoint bound))]))

(define (cut->lower-bound cut)
  (strict-cond
    [(bottom-cut? cut) unbounded]
    [(lower-cut? cut) (inclusive-bound (intermediate-cut-value cut))]
    [(upper-cut? cut) (exclusive-bound (intermediate-cut-value cut))]))

(define (cut->upper-bound cut)
  (strict-cond
    [(top-cut? cut) unbounded]
    [(upper-cut? cut) (inclusive-bound (intermediate-cut-value cut))]
    [(lower-cut? cut) (exclusive-bound (intermediate-cut-value cut))]))

(define/name (cut<=> base-comparator)
  (define (cmp left right)
    (strict-cond
      [(and (bottom-cut? left) (bottom-cut? right)) equivalent]
      [(bottom-cut? left) lesser]
      [(bottom-cut? right) greater]
      [(and (top-cut? left) (top-cut? right)) equivalent]
      [(top-cut? left) greater]
      [(top-cut? right) lesser]
      [else
       (define result
         (compare base-comparator
                  (intermediate-cut-value left)
                  (intermediate-cut-value right)))
       (strict-cond
         [(or (equal? result lesser) (equal? result greater)) result]
         [(and (lower-cut? left) (lower-cut? right)) equivalent]
         [(lower-cut? left) lesser]
         [(lower-cut? right) greater]
         [(and (middle-cut? left) (middle-cut? right)) equivalent]
         [(middle-cut? left) lesser]
         [(middle-cut? right) greater]
         [else equivalent])]))
  (make-comparator cmp #:name enclosing-function-name))

(define (range-contains? rng v)
  (define lower (range-lower-cut rng))
  (define upper (range-upper-cut rng))
  (define cut-v (middle-cut v))
  (define cmp (cut<=> (range-comparator rng)))
  (and (equal? (compare cmp lower cut-v) lesser)
       (equal? (compare cmp cut-v upper) lesser)))

;@------------------------------------------------------------------------------
;; Smart constructors

(define (range lower-bound upper-bound #:comparator [comparator real<=>])
  (constructor:range lower-bound upper-bound comparator))

(define ((range-factory lower-bound-maker upper-bound-maker)
         lower-endpoint upper-endpoint #:comparator [comparator real<=>])
  (range (lower-bound-maker lower-endpoint) (upper-bound-maker upper-endpoint)
         #:comparator comparator))

(define closed-range (range-factory inclusive-bound inclusive-bound))
(define open-range (range-factory exclusive-bound exclusive-bound))
(define closed-open-range (range-factory inclusive-bound exclusive-bound))
(define open-closed-range (range-factory exclusive-bound inclusive-bound))

(define (at-least-range endpoint #:comparator [comparator real<=>])
  (range (inclusive-bound endpoint) unbounded #:comparator comparator))

(define (at-most-range endpoint #:comparator [comparator real<=>])
  (range unbounded (inclusive-bound endpoint) #:comparator comparator))

(define (greater-than-range endpoint #:comparator [comparator real<=>])
  (range (exclusive-bound endpoint) unbounded #:comparator comparator))

(define (less-than-range endpoint #:comparator [comparator real<=>])
  (range unbounded (exclusive-bound endpoint) #:comparator comparator))

(define (singleton-range endpoint #:comparator [comparator real<=>])
  (define bound (inclusive-bound endpoint))
  (range bound bound #:comparator comparator))

(define (unbounded-range #:comparator [comparator real<=>])
  (range unbounded unbounded #:comparator comparator))

(define (unbounded-above-range lower-bound #:comparator [comparator real<=>])
  (range lower-bound unbounded #:comparator comparator))

(define (unbounded-below-range upper-bound #:comparator [comparator real<=>])
  (range unbounded upper-bound #:comparator comparator))

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

  (test-case (name-string at-least-range)
    (define rng (at-least-range 2))
    (check-false (range-contains? rng 1))
    (check-true (range-contains? rng 2))
    (check-true (range-contains? rng 3)))

  (test-case (name-string at-most-range)
    (define rng (at-most-range 2))
    (check-true (range-contains? rng 1))
    (check-true (range-contains? rng 2))
    (check-false (range-contains? rng 3)))

  (test-case (name-string greater-than-range)
    (define rng (greater-than-range 2))
    (check-false (range-contains? rng 1))
    (check-false (range-contains? rng 2))
    (check-true (range-contains? rng 3)))

  (test-case (name-string less-than-range)
    (define rng (less-than-range 2))
    (check-true (range-contains? rng 1))
    (check-false (range-contains? rng 2))
    (check-false (range-contains? rng 3)))

  (test-case (name-string singleton-range)
    (define rng (singleton-range 42))
    (check-true (range-contains? rng 42))
    (check-false (range-contains? rng 41))
    (check-false (range-contains? rng 43)))

  (test-case (name-string unbounded-range)
    (define rng (unbounded-range))
    (check-true (range-contains? rng -100))
    (check-true (range-contains? rng 0))
    (check-true (range-contains? rng 100)))

  (test-case (name-string unbounded-above-range)
    (define rng (unbounded-above-range (inclusive-bound 0)))
    (check-false (range-contains? rng -100))
    (check-true (range-contains? rng 0))
    (check-true (range-contains? rng 100)))

  (test-case (name-string unbounded-below-range)
    (define rng (unbounded-below-range (inclusive-bound 0)))
    (check-true (range-contains? rng -100))
    (check-true (range-contains? rng 0))
    (check-false (range-contains? rng 100))))

;@------------------------------------------------------------------------------
;; Queries

(define (range-encloses? outer inner)
  (define outer-lower (range-lower-cut outer))
  (define outer-upper (range-upper-cut outer))
  (define inner-lower (range-lower-cut inner))
  (define inner-upper (range-upper-cut inner))
  (define cmp (cut<=> (range-comparator outer)))
  (nor (equal? (compare cmp outer-lower inner-lower) greater)
       (equal? (compare cmp outer-upper inner-upper) lesser)))

(module+ test
  (test-case (name-string range-encloses?)

    (test-case "should raise error on unequal comparators"
      (define num-range (closed-range 2 9))
      (define string-range
        (closed-range "apple" "zebra" #:comparator string<=>))
      (check-exn exn:fail:contract?
                 (λ () (range-encloses? num-range string-range))))

    (test-case "closed ranges"
      (define outer (closed-range 2 9))
      (check-true (range-encloses? outer outer))
      (check-true (range-encloses? outer (closed-range 4 6)))
      (check-true (range-encloses? outer (closed-range 2 6)))
      (check-true (range-encloses? outer (closed-range 4 9)))
      (check-true (range-encloses? outer (open-range 2 6)))
      (check-true (range-encloses? outer (open-range 4 9)))
      (check-false (range-encloses? outer (closed-range 1 10)))
      (check-false (range-encloses? outer (closed-range 1 5)))
      (check-false (range-encloses? outer (closed-range 5 10)))
      (check-false (range-encloses? outer (open-range 1 5)))
      (check-false (range-encloses? outer (open-range 5 10)))
      (check-false (range-encloses? outer (greater-than-range 5)))
      (check-false (range-encloses? outer (less-than-range 5)))
      (check-false (range-encloses? outer (at-least-range 5)))
      (check-false (range-encloses? outer (at-most-range 5)))
      (check-true (range-encloses? outer (singleton-range 2)))
      (check-true (range-encloses? outer (singleton-range 5)))
      (check-true (range-encloses? outer (singleton-range 9))))

    (test-case "open ranges"
      (define outer (open-range 2 9))
      (check-true (range-encloses? outer outer))
      (check-true (range-encloses? outer (closed-range 4 6)))
      (check-false (range-encloses? outer (closed-range 2 6)))
      (check-false (range-encloses? outer (closed-range 4 9)))
      (check-true (range-encloses? outer (open-range 2 6)))
      (check-true (range-encloses? outer (open-range 4 9)))
      (check-false (range-encloses? outer (closed-range 1 10)))
      (check-false (range-encloses? outer (closed-range 1 5)))
      (check-false (range-encloses? outer (closed-range 5 10)))
      (check-false (range-encloses? outer (open-range 1 5)))
      (check-false (range-encloses? outer (open-range 5 10)))
      (check-false (range-encloses? outer (greater-than-range 5)))
      (check-false (range-encloses? outer (less-than-range 5)))
      (check-false (range-encloses? outer (at-least-range 5)))
      (check-false (range-encloses? outer (at-most-range 5)))
      (check-false (range-encloses? outer (singleton-range 2)))
      (check-true (range-encloses? outer (singleton-range 5)))
      (check-false (range-encloses? outer (singleton-range 9))))

    (test-case "closed-open ranges"
      (define outer (closed-open-range 2 9))
      (check-true (range-encloses? outer outer))
      (check-true (range-encloses? outer (closed-range 4 6)))
      (check-true (range-encloses? outer (closed-range 2 6)))
      (check-false (range-encloses? outer (closed-range 4 9)))
      (check-true (range-encloses? outer (open-range 2 6)))
      (check-true (range-encloses? outer (open-range 4 9)))
      (check-false (range-encloses? outer (closed-range 1 10)))
      (check-false (range-encloses? outer (closed-range 1 5)))
      (check-false (range-encloses? outer (closed-range 5 10)))
      (check-false (range-encloses? outer (open-range 1 5)))
      (check-false (range-encloses? outer (open-range 5 10)))
      (check-false (range-encloses? outer (greater-than-range 5)))
      (check-false (range-encloses? outer (less-than-range 5)))
      (check-false (range-encloses? outer (at-least-range 5)))
      (check-false (range-encloses? outer (at-most-range 5)))
      (check-true (range-encloses? outer (singleton-range 2)))
      (check-true (range-encloses? outer (singleton-range 5)))
      (check-false (range-encloses? outer (singleton-range 9))))

    (test-case "open-closed ranges"
      (define outer (open-closed-range 2 9))
      (check-true (range-encloses? outer outer))
      (check-true (range-encloses? outer (closed-range 4 6)))
      (check-false (range-encloses? outer (closed-range 2 6)))
      (check-true (range-encloses? outer (closed-range 4 9)))
      (check-true (range-encloses? outer (open-range 2 6)))
      (check-true (range-encloses? outer (open-range 4 9)))
      (check-false (range-encloses? outer (closed-range 1 10)))
      (check-false (range-encloses? outer (closed-range 1 5)))
      (check-false (range-encloses? outer (closed-range 5 10)))
      (check-false (range-encloses? outer (open-range 1 5)))
      (check-false (range-encloses? outer (open-range 5 10)))
      (check-false (range-encloses? outer (greater-than-range 5)))
      (check-false (range-encloses? outer (less-than-range 5)))
      (check-false (range-encloses? outer (at-least-range 5)))
      (check-false (range-encloses? outer (at-most-range 5)))
      (check-false (range-encloses? outer (singleton-range 2)))
      (check-true (range-encloses? outer (singleton-range 5)))
      (check-true (range-encloses? outer (singleton-range 9))))

    (test-case "unbounded-above ranges"
      (define outer (at-least-range 5))
      (check-true (range-encloses? outer outer))
      (check-true (range-encloses? outer (greater-than-range 5)))
      (check-true (range-encloses? outer (at-least-range 9)))
      (check-false (range-encloses? outer (at-least-range 2)))
      (check-true (range-encloses? outer (closed-range 5 9)))
      (check-true (range-encloses? outer (open-range 5 9)))
      (check-false (range-encloses? outer (closed-range 2 9)))
      (check-false (range-encloses? outer (open-range 2 9)))
      (check-false (range-encloses? outer (at-most-range 2)))
      (check-false (range-encloses? outer (at-most-range 5)))
      (check-false (range-encloses? outer (at-most-range 9)))
      (check-false (range-encloses? outer (less-than-range 2)))
      (check-false (range-encloses? outer (less-than-range 5)))
      (check-false (range-encloses? outer (less-than-range 9)))
      (check-false (range-encloses? outer (singleton-range 2)))
      (check-true (range-encloses? outer (singleton-range 5)))
      (check-true (range-encloses? outer (singleton-range 9))))

    (test-case "unbounded-below ranges"
      (define outer (at-most-range 5))
      (check-true (range-encloses? outer outer))
      (check-true (range-encloses? outer (less-than-range 5)))
      (check-true (range-encloses? outer (at-most-range 2)))
      (check-false (range-encloses? outer (at-most-range 9)))
      (check-true (range-encloses? outer (closed-range 2 5)))
      (check-true (range-encloses? outer (open-range 2 5)))
      (check-false (range-encloses? outer (closed-range 2 9)))
      (check-false (range-encloses? outer (open-range 2 9)))
      (check-false (range-encloses? outer (at-least-range 2)))
      (check-false (range-encloses? outer (at-least-range 5)))
      (check-false (range-encloses? outer (at-least-range 9)))
      (check-false (range-encloses? outer (greater-than-range 2)))
      (check-false (range-encloses? outer (greater-than-range 5)))
      (check-false (range-encloses? outer (greater-than-range 9)))
      (check-true (range-encloses? outer (singleton-range 2)))
      (check-true (range-encloses? outer (singleton-range 5)))
      (check-false (range-encloses? outer (singleton-range 9))))))

(define (range-connected? range1 range2)
  (define cmp (cut<=> (range-comparator range1)))
  (define lower1 (range-lower-cut range1))
  (define upper1 (range-upper-cut range1))
  (define lower2 (range-lower-cut range2))
  (define upper2 (range-upper-cut range2))
  (nor (equal? (compare cmp lower1 upper2) greater)
       (equal? (compare cmp lower2 upper1) greater)))

(module+ test
  (test-case (name-string range-connected?)
    (check-true (range-connected? (closed-range 3 6) (closed-range 4 8)))
    (check-true (range-connected? (closed-range 1 9) (closed-range 4 6)))
    (check-true (range-connected? (open-range 1 9) (closed-range 1 4)))
    (check-true (range-connected? (open-range 1 9) (closed-range 4 9)))
    (check-true (range-connected? (closed-range 1 5) (closed-range 5 9)))
    (check-true (range-connected? (open-range 1 5) (closed-range 5 9)))
    (check-true (range-connected? (closed-range 1 5) (open-range 5 9)))
    (check-false (range-connected? (open-range 1 5) (open-range 5 9)))
    (check-false (range-connected? (closed-range 1 2) (closed-range 7 8)))
    (check-false (range-connected? (at-most-range 2) (at-least-range 5)))
    (check-false (range-connected? (less-than-range 4) (greater-than-range 4)))
    (check-true (range-connected? (less-than-range 4) (at-least-range 4)))
    (check-true (range-connected? (at-most-range 4) (greater-than-range 4)))
    (check-true
     (range-connected? (less-than-range 5) (greater-than-range 3)))))

;@------------------------------------------------------------------------------
;; Operations

(define (range-span range1 range2)
  (define lower1 (range-lower-cut range1))
  (define upper1 (range-upper-cut range1))
  (define lower2 (range-lower-cut range2))
  (define upper2 (range-upper-cut range2))
  (define cmp (cut<=> (range-comparator range1)))
  (define lower (if (equal? (compare cmp lower1 lower2) greater) lower2 lower1))
  (define upper (if (equal? (compare cmp upper1 upper2) lesser) upper2 upper1))
  (range (cut->lower-bound lower)
         (cut->upper-bound upper)
         #:comparator (range-comparator range1)))

(module+ test
  (test-case (name-string range-span)
    (check-exn
     exn:fail:contract?
     (λ ()
       (range-span (at-most-range 7)
                   (at-most-range "apple" #:comparator string<=>))))
    (check-equal? (range-span (closed-range 2 4) (open-range 8 16))
                  (closed-open-range 2 16))
    (check-equal? (range-span (open-range 3 8) (closed-range 3 5))
                  (closed-open-range 3 8))
    (check-equal? (range-span (open-range 2 9) (closed-range 4 6))
                  (open-range 2 9))
    (check-equal? (range-span (closed-open-range 2 5) (open-closed-range 7 9))
                  (closed-range 2 9))
    (check-equal? (range-span (open-closed-range 2 5) (closed-open-range 7 9))
                  (open-range 2 9))
    (check-equal? (range-span (at-least-range 2) (closed-range 5 6))
                  (at-least-range 2))
    (check-equal? (range-span (at-least-range 4) (closed-range 3 8))
                  (at-least-range 3))
    (check-equal? (range-span (at-least-range 4) (open-range 3 8))
                  (greater-than-range 3))
    (check-equal? (range-span (less-than-range 4) (greater-than-range 2))
                  (range unbounded unbounded))
    (check-equal? (range-span (greater-than-range 4) (less-than-range 2))
                  (range unbounded unbounded))
    (check-equal? (range-span (less-than-range 7) (closed-range 2 5))
                  (less-than-range 7))
    (check-equal? (range-span (less-than-range 7) (open-range 2 7))
                  (less-than-range 7))
    (check-equal? (range-span (less-than-range 7) (closed-range 2 7))
                  (at-most-range 7))))

;@------------------------------------------------------------------------------
;; Contract helpers

(define (default-real<=> v) (if (unsupplied-arg? v) real<=> v))

(define single-endpoint-range-constructor/c
  (->* (any/c) (#:comparator comparator?) range?))
