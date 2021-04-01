#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [range-set? predicate/c]
  [range-set (-> nonempty-range? ... range-set?)]
  [range-set-size (-> range-set? natural?)]
  [sequence->range-set (-> (sequence/c nonempty-range?) range-set?)]
  [into-range-set (reducer/c nonempty-range? range-set?)]
  [in-range-set (-> range-set? (sequence/c nonempty-range?))]
  [empty-range-set empty-range-set?]
  [empty-range-set? predicate/c]
  [nonempty-range-set? predicate/c]
  [range-set-contains? (-> range-set? any/c boolean?)]
  [range-set-encloses? (-> range-set? range? boolean?)]
  [range-set-encloses-all? (-> range-set? (or/c range-set? (sequence/c range?)) boolean?)]
  [range-subset (-> range-set? range? range-set?)]))


(require racket/math
         racket/sequence
         racket/struct
         racket/vector
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/vector/builder
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------
;; Vector utilities


;; Vector A, BiPredicate A, BinaryOperator A -> ImmutableVector A
;; Returns a new vector that is like vec, except adjacent elements are merged with merge-function when
;; should-merge? returns true.
;; Examples:
;; > (vector-merge-adjacent (vector 1 2 3 "hello" 4 5 "world" 6) both-numbers? +)
;; (vector 6 "hello" 9 "world" 6)
(define (vector-merge-adjacent vec should-merge? merge-function)
  (define count (vector-length vec))
  (define/guard (iteration-loop builder i)
    (guard (equal? i count) then
      (build-vector builder))
    (define/guard (merge-loop element j)
      (guard (equal? j count) then
        (build-vector (vector-builder-add builder element)))
      (define next-element (vector-ref vec j))
      (if (should-merge? element next-element)
          (merge-loop (merge-function element next-element) (add1 j))
          (iteration-loop (vector-builder-add builder element) j)))
    (merge-loop (vector-ref vec i) (add1 i)))
  (iteration-loop (make-vector-builder #:expected-size count) 0))


;@----------------------------------------------------------------------------------------------------
;; Data definition


(struct range-set (sorted-range-vector)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:range-set
  #:property prop:sequence (λ (this) (range-set-sorted-range-vector this))

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (this) 'range-set)
      (λ (this) (range-set-sorted-range-vector this))))])


(define (range-set . ranges)
  (sequence->range-set ranges))


(define (sequence->range-set ranges)
  (transduce ranges #:into into-range-set))


(define (empty-range-set? v)
  (and (range-set? v) (zero? (vector-length (range-set-sorted-range-vector v)))))


(define (nonempty-range-set? v)
  (and (range-set? v) (not (zero? (vector-length (range-set-sorted-range-vector v))))))


(define (in-range-set range-set)
  range-set)


(struct range-set-builder ([range-vector-builder #:mutable]))


(define (make-range-set-builder)
  (range-set-builder (make-vector-builder)))


(define (range-set-builder-add-range builder range)
  (define vector-builder (vector-builder-add (range-set-builder-range-vector-builder builder) range))
  (set-range-set-builder-range-vector-builder! builder vector-builder)
  builder)


(define (build-range-set builder)
  (define ranges (build-vector (range-set-builder-range-vector-builder builder)))
  (check-ranges-use-same-comparator #:who (name build-range-set) ranges)
  (define sorted-ranges (vector-sort ranges range<?))
  (check-ranges-disjoint #:who (name build-range-set) sorted-ranges)
  (define coalesced-ranges (vector-merge-adjacent sorted-ranges range-connected? range-span))
  (constructor:range-set coalesced-ranges))


(define (check-ranges-use-same-comparator #:who who ranges)
  (unless (zero? (vector-length ranges))
    (for ([range (in-vector ranges)]
          [next-range (sequence-tail (in-vector ranges) 1)])
      (unless (equal? (range-comparator range) (range-comparator next-range))
        (raise-arguments-error
         who
         "not all ranges use the same comparator"
         "range" range
         "next range" next-range)))))


(define (range<? range other-range)
  (equal? (compare range<=> range other-range) lesser))


(define (check-ranges-disjoint #:who who ranges)
  (unless (zero? (vector-length ranges))
    (for ([range (in-vector ranges)]
          [next-range (sequence-tail (in-vector ranges) 1)])
      (when (range-overlaps? range next-range)
        (raise-arguments-error
         who
         "overlapping ranges not allowed"
         "range" range
         "next range" next-range)))))


(define into-range-set
  (make-effectful-fold-reducer
   range-set-builder-add-range make-range-set-builder build-range-set #:name (name into-range-set)))


(define empty-range-set (range-set))


(module+ test

  (test-case "empty range sets"
    (check-equal? empty-range-set (range-set)))

  (test-case "nonempty range sets"

    (test-case "single range"
      (define actual (range-set (closed-range 1 4)))
      (check-equal? actual (range-set (closed-range 1 4)))
      (check-not-equal? actual (range-set (closed-range 5 8)))
      (check-not-equal? actual empty-range-set))

    (test-case "two disconnected ranges"
      (define actual (range-set (closed-range 1 3) (closed-range 6 9)))
      (check-equal? actual (range-set (closed-range 6 9) (closed-range 1 3)))
      (check-not-equal? actual (range-set (closed-range 1 3)))
      (check-not-equal? actual (range-set (closed-range 6 9))))

    (test-case "many disconnected ranges"
      (define actual
        (range-set
         (closed-range 1 3)
         (closed-range 6 9)
         (closed-range 12 14)
         (closed-range 18 20)))
      (check-equal? actual
                    (range-set
                     (closed-range 1 3)
                     (closed-range 6 9)
                     (closed-range 12 14)
                     (closed-range 18 20)))
      (check-not-equal? actual (range-set (closed-range 1 3) (closed-range 6 9)))
      (check-not-equal? actual (range-set (closed-range 12 14) (closed-range 18 20)))

      (test-case "unordered"
        (define expected-equivalent-set
          (range-set
           (closed-range 18 20)
           (closed-range 6 9)
           (closed-range 12 14)
           (closed-range 1 3)))
        (check-equal? actual expected-equivalent-set)))

    (test-case "two connected ranges"
      (define actual (range-set (closed-open-range 1 3) (closed-open-range 3 5)))
      (check-equal? actual (range-set (closed-open-range 1 5)))
      (check-equal? actual (range-set (closed-open-range 3 5) (closed-open-range 1 3)))
      (check-not-equal? actual (range-set (closed-open-range 1 3) (open-range 3 5))))

    (test-case "many connected ranges"
      (define actual
        (range-set
         (closed-open-range 1 3)
         (closed-open-range 3 5)
         (closed-open-range 5 7)
         (closed-open-range 7 10)))
      (check-equal? actual (range-set (closed-open-range 1 10)))

      (test-case "unordered"
        (define expected-equivalent-set
          (range-set
           (closed-open-range 7 10)
           (closed-open-range 1 3)
           (closed-open-range 5 7)
           (closed-open-range 3 5)))
        (check-equal? actual expected-equivalent-set))

      (test-case "equivalent but differently coalesced range set"
        (define expected-equivalent-set (range-set (singleton-range 1) (open-range 1 10)))
        (check-equal? actual expected-equivalent-set))))

  (test-case "range set sequences"
    (define ranges (range-set (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (define expected (list (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (check-equal? (sequence->list ranges) expected)))


;@----------------------------------------------------------------------------------------------------
;; Queries


(define (range-set-size ranges)
  (vector-length (range-set-sorted-range-vector ranges)))


(define (range-set-contains? ranges value)
  #false)


(define (range-set-encloses? ranges other-range)
  #false)


(define (range-set-encloses-all? ranges other-ranges)
  #false)


(define (range-subset ranges subset-range)
  ranges)
