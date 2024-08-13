#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [range? predicate/c]
  [bounded-range? predicate/c]
  [bounded-below-range? predicate/c]
  [bounded-above-range? predicate/c]
  [unbounded-range? predicate/c]
  [unbounded-below-range? predicate/c]
  [unbounded-above-range? predicate/c]
  [singleton-range? predicate/c]
  [empty-range? predicate/c]
  [nonempty-range? predicate/c]

  [range
   (->i #:chaperone
        ([lower-bound (or/c range-bound? unbounded?)]
         [upper-bound (or/c range-bound? unbounded?)])
        (#:comparator [cmp comparator?])

        #:pre/name (lower-bound upper-bound cmp)
        "lower endpoint must be less than or equal to upper endpoint"
        (guarded-block
          (guard (not (unbounded? lower-bound)) #:else
            #true)
          (guard (not (unbounded? upper-bound)) #:else
            #true)
          (define lower (range-bound-endpoint lower-bound))
          (define upper (range-bound-endpoint upper-bound))
          (not (equal? (compare (default-real<=> cmp) lower upper) greater)))

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

        [_ bounded-range?])]

  [open-range
   (->i #:chaperone
        ([lower any/c]
         [upper any/c])
        (#:comparator [cmp comparator?])

        #:pre/name (lower upper cmp)
        "lower endpoint must be less than upper endpoint"
        (equal? (compare (default-real<=> cmp) lower upper) lesser)

        [_ bounded-range?])]

  [closed-open-range
   (->i #:chaperone
        ([lower any/c]
         [upper any/c])
        (#:comparator [cmp comparator?])

        #:pre/name (lower upper cmp)
        "lower endpoint must be less than or equal to upper endpoint"
        (not (equal? (compare (default-real<=> cmp) lower upper) greater))

        [_ bounded-range?])]

  [open-closed-range
   (->i #:chaperone
        ([lower any/c]
         [upper any/c])
        (#:comparator [cmp comparator?])

        #:pre/name (lower upper cmp)
        "lower endpoint must be less than or equal to upper endpoint"
        (not (equal? (compare (default-real<=> cmp) lower upper) greater))

        [_ bounded-range?])]

  [at-least-range
   (single-endpoint-range-constructor/c
    (and/c unbounded-above-range? bounded-below-range?))]
  [at-most-range
   (single-endpoint-range-constructor/c
    (and/c unbounded-below-range? bounded-above-range?))]
  [less-than-range
   (single-endpoint-range-constructor/c
    (and/c unbounded-below-range? bounded-above-range?))]
  [greater-than-range
   (single-endpoint-range-constructor/c
    (and/c unbounded-above-range? bounded-below-range?))]
  [singleton-range
   (single-endpoint-range-constructor/c (and/c bounded-range? nonempty-range?))]
  [unbounded-range (->* () (#:comparator comparator?) unbounded-range?)]
  [unbounded-above-range
   (->* (range-bound?) (#:comparator comparator?) unbounded-above-range?)]
  [unbounded-below-range
   (->* (range-bound?) (#:comparator comparator?) unbounded-below-range?)]
  [range-contains? (-> range? any/c boolean?)]
  [range-encloses? binary-range-predicate/c]
  [range-connected? binary-range-predicate/c]
  [range-overlaps? binary-range-predicate/c]
  [range-span binary-range-operator/c]
  [range-gap binary-range-operator/c]
  [range-intersection binary-range-operator/c]
  [range-lower-bound (-> range? (or/c range-bound? unbounded?))]
  [range-upper-bound (-> range? (or/c range-bound? unbounded?))]
  [range-lower-endpoint (-> bounded-below-range? any/c)]
  [range-upper-endpoint (-> bounded-above-range? any/c)]
  [range-comparator (-> range? comparator?)]
  [unbounded? predicate/c]
  [unbounded unbounded?]
  [bound-type? predicate/c]
  [inclusive bound-type?]
  [exclusive bound-type?]
  [range-bound? predicate/c]
  [range-bound (-> any/c bound-type? range-bound?)]
  [range-bound-endpoint (-> range-bound? any/c)]
  [range-bound-type (-> range-bound? bound-type?)]
  [inclusive-bound (-> any/c range-bound?)]
  [exclusive-bound (-> any/c range-bound?)]
  [range<=> (comparator/c range?)]))


(module+ private-for-rebellion-only
  (provide range-compare-to-cut
           range-compare-to-value
           range-lower-cut
           range-upper-cut
           range-from-cuts))


(require racket/bool
         racket/match
         rebellion/base/comparator
         rebellion/private/cut
         guard
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


(struct range (lower-bound upper-bound comparator)
  #:transparent
  #:constructor-name constructor:range
  #:omit-define-syntaxes

  #:methods gen:custom-write

  [(define (write-proc this out mode)
     (define (recur v)
       (match mode
         [#true (write v out)]
         [#false (display v out)]
         [0 (print v out 0)]
         [1 (print v out 1)]))
     (write-string "#<range:" out)
     (write-string (symbol->string (object-name (range-comparator this))) out)
     (write-string " " out)
     (match (range-lower-bound this)
       [(== unbounded)
        (write-string "[-∞" out)]
       [(inclusive-bound lower)
        (write-string "[" out)
        (recur lower)]
       [(exclusive-bound lower)
        (write-string "(" out)
        (recur lower)])
     (write-string ", " out)
     (match (range-upper-bound this)
       [(== unbounded)
        (write-string "∞]" out)]
       [(inclusive-bound upper)
        (recur upper)
        (write-string "]" out)]
       [(exclusive-bound upper)
        (recur upper)
        (write-string ")" out)])
     (write-string ">" out))])


(define-singleton-type unbounded)


(define-enum-type bound-type (inclusive exclusive))


(define-tuple-type inclusive-bound (endpoint))
(define-tuple-type exclusive-bound (endpoint))


(define (range-bound? v)
  (or (inclusive-bound? v) (exclusive-bound? v)))


(define (range-bound endpoint type)
  (if (equal? type inclusive)
      (inclusive-bound endpoint)
      (exclusive-bound endpoint)))


(define (range-bound-type bound)
  (if (inclusive-bound? bound) inclusive exclusive))


(define (range-bound-endpoint bound)
  (if (inclusive-bound? bound)
      (inclusive-bound-endpoint bound)
      (exclusive-bound-endpoint bound)))


(define (range-lower-endpoint range)
  (range-bound-endpoint (range-lower-bound range)))


(define (range-upper-endpoint range)
  (range-bound-endpoint (range-upper-bound range)))


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


(define (range-from-cuts lower-cut upper-cut #:comparator comparator)
  (range (cut->lower-bound lower-cut) (cut->upper-bound upper-cut) #:comparator comparator))


(define (cut->lower-bound cut)
  (match cut
    [(== bottom-cut) unbounded]
    [(lower-cut endpoint) (inclusive-bound endpoint)]
    [(upper-cut endpoint) (exclusive-bound endpoint)]))


(define (cut->upper-bound cut)
  (match cut
    [(== top-cut) unbounded]
    [(upper-cut endpoint) (inclusive-bound endpoint)]
    [(lower-cut endpoint) (exclusive-bound endpoint)]))


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

(define (bounded-range? range)
  (and (range? range)
       (nor (unbounded? (range-lower-bound range))
            (unbounded? (range-upper-bound range)))))

(define (bounded-below-range? range)
  (and (range? range) (not (unbounded? (range-lower-bound range)))))

(define (bounded-above-range? range)
  (and (range? range) (not (unbounded? (range-upper-bound range)))))

(define (unbounded-range? range)
  (and (range? range)
       (or (unbounded? (range-lower-bound range))
           (unbounded? (range-upper-bound range)))))

(define (unbounded-below-range? range)
  (and (range? range)
       (unbounded? (range-lower-bound range))))

(define (unbounded-above-range? range)
  (and (range? range)
       (unbounded? (range-upper-bound range))))

(define (singleton-range? range)
  (and (bounded-range? range)
       (equal? (range-lower-endpoint range)
               (range-upper-endpoint range))
       (and (inclusive-bound? (range-lower-bound range))
            (inclusive-bound? (range-upper-bound range)))))

(define (empty-range? range)
  (and (bounded-range? range)
       (equal? (range-lower-endpoint range)
               (range-upper-endpoint range))
       (xor (inclusive-bound? (range-lower-bound range))
            (inclusive-bound? (range-upper-bound range)))))

(define/guard (nonempty-range? range)
  (guard (range? range) #:else
    #false)
  (define lower (range-lower-bound range))
  (define upper (range-upper-bound range))
  (or (unbounded? lower)
      (unbounded? upper)
      (and (inclusive-bound? lower) (inclusive-bound? upper))
      (not (equal? (range-bound-endpoint lower)
                   (range-bound-endpoint upper)))))

(define (range-encloses? outer inner)
  (define outer-lower (range-lower-cut outer))
  (define outer-upper (range-upper-cut outer))
  (define inner-lower (range-lower-cut inner))
  (define inner-upper (range-upper-cut inner))
  (define cmp (cut<=> (range-comparator outer)))
  (nor (equal? (compare cmp outer-lower inner-lower) greater)
       (equal? (compare cmp outer-upper inner-upper) lesser)))


(define range<=>
  (make-comparator
   (λ (range other-range)
     (unless (equal? (range-comparator range) (range-comparator other-range))
       (raise-arguments-error
        (name range<=>) "ranges must use same comparator" "range" range "other range" other-range))
     (define cmp (cut<=> (range-comparator range)))
     (match (compare cmp (range-lower-cut range) (range-lower-cut other-range))
       [(== lesser) lesser]
       [(== greater) greater]
       [(== equivalent) (compare cmp (range-upper-cut range) (range-upper-cut other-range))]))
   #:name (name range<=>)))


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
      (check-false (range-encloses? outer (singleton-range 9)))))

  (test-case (name-string range<=>)

    (test-case "should raise error on unequal comparators"
      (define num-range (closed-range 2 9))
      (define string-range
        (closed-range "apple" "zebra" #:comparator string<=>))
      (check-exn exn:fail:contract?
                 (λ () (compare range<=> num-range string-range))))

    (test-case "equal ranges compare equivalent"
      (check-equal? (compare range<=> (closed-range 2 5) (closed-range 2 5)) equivalent)
      (check-equal? (compare range<=> (open-range 2 5) (open-range 2 5)) equivalent)
      (check-equal? (compare range<=> (closed-open-range 2 5) (closed-open-range 2 5)) equivalent)
      (check-equal? (compare range<=> (open-closed-range 2 5) (open-closed-range 2 5)) equivalent)
      (check-equal? (compare range<=> (less-than-range 2) (less-than-range 2)) equivalent)
      (check-equal? (compare range<=> (greater-than-range 2) (greater-than-range 2)) equivalent)
      (check-equal? (compare range<=> (at-most-range 2) (at-most-range 2)) equivalent)
      (check-equal? (compare range<=> (at-least-range 2) (at-least-range 2)) equivalent)
      (check-equal? (compare range<=> (singleton-range 2) (singleton-range 2)) equivalent))

    (test-case "disconnected ranges compare based on their endpoints"
      (check-equal? (compare range<=> (closed-range 2 5) (closed-range 6 10)) lesser)
      (check-equal? (compare range<=> (less-than-range 2) (closed-range 6 10)) lesser)
      (check-equal? (compare range<=> (greater-than-range 6) (less-than-range 2)) greater)
      (check-equal? (compare range<=> (singleton-range 6) (less-than-range 2)) greater))

    (test-case "overlapping ranges compare based on their minimum endpoints"
      (check-equal? (compare range<=> (closed-range 2 6) (closed-range 4 10)) lesser)
      (check-equal? (compare range<=> (closed-range 4 10) (closed-range 2 6)) greater)
      (check-equal? (compare range<=> (closed-range 2 5) (at-least-range 3)) lesser)
      (check-equal? (compare range<=> (at-least-range 3) (closed-range 2 5)) greater)
      (check-equal? (compare range<=> (closed-range 2 5) (at-least-range 1)) greater)
      (check-equal? (compare range<=> (at-least-range 1) (closed-range 2 5)) lesser)
      (check-equal? (compare range<=> (closed-range 2 5) (open-closed-range 2 5)) lesser)
      (check-equal? (compare range<=> (at-least-range 4) (greater-than-range 4)) lesser))

    (test-case "for ranges with equal minimum endpoints, the lesser range is the smaller one"
      (check-equal? (compare range<=> (closed-range 2 6) (closed-range 2 10)) lesser)
      (check-equal? (compare range<=> (closed-range 2 10) (closed-range 2 6)) greater)
      (check-equal? (compare range<=> (at-least-range 2) (closed-range 2 5)) greater)
      (check-equal? (compare range<=> (closed-range 2 5) (at-least-range 2)) lesser))

    (test-case "for ranges with equal maximum endpoints, the lesser range is the larger one"
      (check-equal? (compare range<=> (closed-range 2 10) (closed-range 6 10)) lesser)
      (check-equal? (compare range<=> (closed-range 6 10) (closed-range 2 10)) greater)
      (check-equal? (compare range<=> (at-most-range 5) (closed-range 2 5)) lesser)
      (check-equal? (compare range<=> (closed-range 2 5) (at-most-range 5)) greater))))


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


(define (range-overlaps? range1 range2)
  (define cmp (cut<=> (range-comparator range1)))
  (and (equal? (compare cmp (range-upper-cut range1) (range-lower-cut range2)) greater)
       (equal? (compare cmp (range-lower-cut range1) (range-upper-cut range2)) lesser)))


(module+ test
  (test-case (name-string range-overlaps?)
    (check-true (range-overlaps? (closed-range 1 5) (closed-range 2 10)))
    (check-true (range-overlaps? (closed-range 2 10) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 5) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 5) (closed-range 2 3)))
    (check-true (range-overlaps? (closed-range 2 3) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 5) (closed-range 5 10)))
    (check-true (range-overlaps? (closed-range 5 10) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 5) (closed-range 1 3)))
    (check-true (range-overlaps? (closed-range 1 3) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 5) (closed-range 3 5)))
    (check-true (range-overlaps? (closed-range 3 5) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 5) (open-range 1 3)))
    (check-true (range-overlaps? (open-range 1 3) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 5) (open-range 3 5)))
    (check-true (range-overlaps? (open-range 3 5) (closed-range 1 5)))
    (check-false (range-overlaps? (closed-range 1 5) (closed-range 10 20)))
    (check-false (range-overlaps? (closed-range 10 20) (closed-range 1 5)))
    (check-false (range-overlaps? (closed-range 1 5) (open-range 5 10)))
    (check-false (range-overlaps? (open-range 5 10) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 10) (closed-range 1 10)))
    (check-true (range-overlaps? (open-range 1 10) (open-range 1 10)))
    (check-true (range-overlaps? (closed-open-range 1 10) (closed-open-range 1 10)))
    (check-true (range-overlaps? (open-closed-range 1 10) (open-closed-range 1 10)))
    (check-true (range-overlaps? (singleton-range 5) (singleton-range 5)))
    (check-true (range-overlaps? (singleton-range 5) (closed-range 5 10)))
    (check-true (range-overlaps? (closed-range 5 10) (singleton-range 5)))
    (check-true (range-overlaps? (singleton-range 5) (closed-range 1 5)))
    (check-true (range-overlaps? (closed-range 1 5) (singleton-range 5)))
    (check-false (range-overlaps? (singleton-range 5) (open-range 5 10)))
    (check-false (range-overlaps? (open-range 5 10) (singleton-range 5)))
    (check-false (range-overlaps? (singleton-range 5) (open-range 1 5)))
    (check-false (range-overlaps? (open-range 1 5) (singleton-range 5)))))


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
  (range-from-cuts lower upper #:comparator (range-comparator range1)))


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


(define/guard (range-gap range1 range2)
  (define cmp (cut<=> (range-comparator range1)))
  (guard (equal? (compare cmp (range-upper-cut range1) (range-lower-cut range2)) greater) #:else
    (range
     (range-bound-flip (range-upper-bound range1))
     (range-bound-flip (range-lower-bound range2))
     #:comparator (range-comparator range1)))
  (guard (equal? (compare cmp (range-lower-cut range1) (range-upper-cut range2)) lesser) #:else
    (range
     (range-bound-flip (range-upper-bound range2))
     (range-bound-flip (range-lower-bound range1))
     #:comparator (range-comparator range1)))
  (raise-arguments-error
   (name range-gap)
   "expected non-overlapping ranges"
   "range1" range1
   "range2" range2))


(define (range-bound-flip bound)
  (match bound
    [(== unbounded) unbounded]
    [(inclusive-bound endpoint) (exclusive-bound endpoint)]
    [(exclusive-bound endpoint) (inclusive-bound endpoint)]))


(define (range-intersection range1 range2)
  (unless (range-connected? range1 range2)
    (raise-arguments-error
     (name range-intersection)
     "expected connected ranges"
     "range1" range1
     "range2" range2))
  (define cmp (cut<=> (range-comparator range1)))
  (define lower-bound
    (match (compare cmp (range-lower-cut range1) (range-lower-cut range2))
      [(== lesser) (range-lower-bound range2)]
      [_ (range-lower-bound range1)]))
  (define upper-bound
    (match (compare cmp (range-upper-cut range1) (range-upper-cut range2))
      [(== greater) (range-upper-bound range2)]
      [_ (range-upper-bound range1)]))
  (range lower-bound upper-bound #:comparator (range-comparator range1)))


(module+ test
  (test-case (name-string range-gap)
    (check-equal? (range-gap (closed-range 2 7) (closed-range 10 15)) (open-range 7 10))
    (check-equal? (range-gap (closed-range 10 15) (closed-range 2 7)) (open-range 7 10))
    (check-equal? (range-gap (less-than-range 7) (greater-than-range 10)) (closed-range 7 10))
    (check-equal? (range-gap (greater-than-range 10) (less-than-range 7)) (closed-range 7 10))
    (check-exn
     #rx"expected non-overlapping ranges" (λ () (range-gap (closed-range 2 10) (closed-range 5 7))))
    (check-exn
     #rx"expected non-overlapping ranges" (λ () (range-gap (closed-range 5 7) (closed-range 2 10))))
    (check-exn
     #rx"expected non-overlapping ranges"
     (λ () (range-gap (less-than-range 7) (greater-than-range 5))))
    (check-exn
     #rx"expected non-overlapping ranges"
     (λ () (range-gap (greater-than-range 5) (less-than-range 7))))
    (check-equal? (range-gap (closed-range 2 7) (open-range 7 10)) (open-closed-range 7 7))
    (check-equal? (range-gap (open-range 7 10) (closed-range 2 7)) (open-closed-range 7 7))
    (check-equal? (range-gap (open-range 2 7) (closed-range 7 10)) (closed-open-range 7 7))
    (check-equal? (range-gap (closed-range 7 10) (open-range 2 7)) (closed-open-range 7 7))
    (check-exn
     #rx"expected non-overlapping ranges"
     (λ () (range-gap (closed-range 2 7) (closed-range 7 10)))))

  (test-case (name-string range-intersection)
    (check-equal? (range-intersection (closed-range 2 8) (closed-range 5 10)) (closed-range 5 8))
    (check-equal? (range-intersection (closed-range 5 10) (closed-range 2 8)) (closed-range 5 8))
    (check-equal? (range-intersection (closed-range 0 10) (closed-range 4 6)) (closed-range 4 6))
    (check-equal? (range-intersection (closed-range 4 6) (closed-range 0 10)) (closed-range 4 6))
    (check-equal? (range-intersection (closed-range 0 5) (closed-range 5 10)) (singleton-range 5))
    (check-equal? (range-intersection (closed-range 5 10) (closed-range 0 5)) (singleton-range 5))
    (check-equal? (range-intersection (open-range 0 5) (closed-range 5 10)) (closed-open-range 5 5))
    (check-equal? (range-intersection (closed-range 5 10) (open-range 0 5)) (closed-open-range 5 5))
    (check-equal? (range-intersection (closed-range 0 5) (open-range 5 10)) (open-closed-range 5 5))
    (check-equal? (range-intersection (open-range 5 10) (closed-range 0 5)) (open-closed-range 5 5))))


;@------------------------------------------------------------------------------
;; Private utilities exported only for range set implementation


(define (range-compare-to-cut range cut)
  (define lower (range-lower-cut range))
  (define upper (range-upper-cut range))
  (define cmp (cut<=> (range-comparator range)))
  (match (compare cmp cut lower)
    [(== equivalent) equivalent]
    [(== lesser) greater]
    [(== greater)
     (match (compare cmp cut upper)
       [(== equivalent) equivalent]
       [(== lesser) equivalent]
       [(== greater) lesser])]))


(define (range-compare-to-value range value)
  (range-compare-to-cut range (middle-cut value)))


(module+ test
  (test-case (name-string range-compare-to-value)

    (test-case "singleton ranges"
      (define r (singleton-range 4))
      (check-equal? (range-compare-to-value r 4) equivalent)
      (check-equal? (range-compare-to-value r 2) greater)
      (check-equal? (range-compare-to-value r 6) lesser))))


;@------------------------------------------------------------------------------
;; Contract helpers


(define (default-real<=> v) (if (unsupplied-arg? v) real<=> v))


(define (single-endpoint-range-constructor/c range-subtype)
  (->* (any/c) (#:comparator comparator?) range-subtype))


(define binary-range-predicate/c
  (->i #:chaperone
       ([range1 range?] [range2 range?])

       #:pre/name (range1 range2)
       "both ranges must use the same comparator"
       (equal? (range-comparator range1) (range-comparator range2))

       [_ boolean?]))


(define binary-range-operator/c
  (->i #:chaperone
       ([range1 range?] [range2 range?])

       #:pre/name (range1 range2)
       "both ranges must use the same comparator"
       (equal? (range-comparator range1) (range-comparator range2))

       [_ range?]))
