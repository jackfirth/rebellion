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


(require racket/match
         racket/math
         racket/sequence
         racket/struct
         (only-in racket/unsafe/ops unsafe-vector*->immutable-vector!)
         racket/vector
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-range-set-implementation-only)
         rebellion/collection/vector/builder
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/tuple)


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
(define/guard (vector-merge-adjacent vec should-merge? merge-function)
  (define count (vector-length vec))
  (guard (< count 2) then
    (vector->immutable-vector vec))
  (for/fold ([builder (make-vector-builder #:expected-size count)]
             [element (vector-ref vec 0)]
             #:result (build-vector (vector-builder-add builder element)))
            ([next-element (in-vector vec 1)])
    (if (should-merge? element next-element)
        (values builder (merge-function element next-element))
        (values (vector-builder-add builder element) next-element))))


(module+ test
  (test-case (name-string vector-merge-adjacent)

    (define (both-numbers? left right)
      (and (number? left) (number? right)))

    (define (fail-immediately left right)
      (raise 'should-not-be-called))

    (test-case "empty vectors are returned uninspected"
      (define actual (vector-merge-adjacent (vector-immutable) fail-immediately fail-immediately))
      (check-equal? actual (vector-immutable)))

    (test-case "single-element vectors are returned uninspected"
      (define actual (vector-merge-adjacent (vector-immutable 1) fail-immediately fail-immediately))
      (check-equal? actual (vector-immutable 1)))

    (test-case "can merge all elements"
      (define actual (vector-merge-adjacent (vector-immutable 1 2 3 4 5) (λ (a b) #true) +))
      (check-equal? actual (vector-immutable 15)))

    (test-case "can merge no elements"
      (define actual
        (vector-merge-adjacent (vector-immutable 1 2 3 4 5) (λ (a b) #false) fail-immediately))
      (check-equal? actual (vector-immutable 1 2 3 4 5)))

    (test-case "can merge elements at start"
      (define actual (vector-merge-adjacent (vector-immutable 1 2 3 'a 'b 'c) both-numbers? +))
      (check-equal? actual (vector-immutable 6 'a 'b 'c)))

    (test-case "can merge elements at end"
      (define actual (vector-merge-adjacent (vector-immutable 'a 'b 'c 4 5 6) both-numbers? +))
      (check-equal? actual (vector-immutable 'a 'b 'c 15)))

    (test-case "can merge elements in middle"
      (define actual
        (vector-merge-adjacent (vector-immutable 'a 'b 'c 4 5 6 'd 'e 'f) both-numbers? +))
      (check-equal? actual (vector-immutable 'a 'b 'c 15 'd 'e 'f)))

    (test-case "can merge elements in middle multiple times"
      (define actual
        (vector-merge-adjacent (vector-immutable 1 2 3 "hello" 4 5 "world" 6) both-numbers? +))
    (check-equal? actual (vector-immutable 6 "hello" 9 "world" 6)))))


(define-tuple-type exact-match (index))

;; The position indicates where in the vector the search ended up. It's not an index, it's a position
;; *between* indices, so its range is [0, vector-length + 1] inclusive.
(define-tuple-type no-match (position))

;; Vector A, (A -> Comparison) -> (Or ExactMatch NoMatch)
;; Searches vec for an element for which the search function returns the `equivalent` constant, then
;; returns the index of that element if such an element exists. The vector is assumed to be sorted in
;; a manner consistent with the comparison results returned by the search function. The search is a
;; binary search taking log(n) time. Note that if there are multiple elements for which the search
;; function returns `equivalent`, it is unspecified which of them is chosen. If no elements satisfy
;; the search function, `absent` is returned.
;; Examples:
;; > (vector-binary-search (vector "a" "b" "d" "e") (λ (x) (compare string<=> x "d")))
;; (exact-match 2)
;; > (vector-binary-search (vector "a" "b" "d" "e") (λ (x) (compare string<=> x "c")))
;; (no-match 2)
(define (vector-binary-search vec search-function)
  (define/guard (loop [lower 0] [upper (sub1 (vector-length vec))])
    (guard (<= lower upper) else
      (no-match lower))
    (define middle (quotient (+ lower upper) 2))
    (match (search-function (vector-ref vec middle))
      [(== lesser) (loop (add1 middle) upper)]
      [(== greater) (loop lower (sub1 middle))]
      [(== equivalent) (exact-match middle)]))
  (loop))


(module+ test
  (test-case (name-string vector-binary-search)
    (define vec (vector "aa" "bb" "cc" "dd"))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "aa"))) (exact-match 0))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "bb"))) (exact-match 1))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "cc"))) (exact-match 2))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "dd"))) (exact-match 3))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "a"))) (no-match 0))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "b"))) (no-match 1))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "c"))) (no-match 2))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "d"))) (no-match 3))
    (check-equal? (vector-binary-search vec (λ (x) (compare string<=> x "e"))) (no-match 4))
    (check-equal? (vector-binary-search (vector) (λ (x) (compare string<=> x "a"))) (no-match 0))))


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
    (check-equal? (sequence->list ranges) expected))

  (test-case "sequence->range-set"
    (define ranges (list (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (define expected (range-set (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (check-equal? (sequence->range-set ranges) expected))

  (test-case "into-range-set"
    (define ranges (list (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (define expected (range-set (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (check-equal? (transduce ranges #:into into-range-set) expected)))


;@----------------------------------------------------------------------------------------------------
;; Queries


(define (range-set-size ranges)
  (vector-length (range-set-sorted-range-vector ranges)))


(define (range-set-contains? ranges value)
  (define vec (range-set-sorted-range-vector ranges))
  (exact-match? (vector-binary-search vec (λ (range) (range-compare-to-value range value)))))


(define (range-set-encloses? ranges other-range)
  (define vec (range-set-sorted-range-vector ranges))
  (match (vector-binary-search vec (λ (range) (range-compare-to-range range other-range)))
    [(exact-match overlapping-range-index)
     (range-encloses? (vector-ref vec overlapping-range-index) other-range)]
    [_ #false]))


(define (range-set-encloses-all? ranges other-ranges)
  #false)


(define (range-subset ranges subset-range)
  (define vec (range-set-sorted-range-vector ranges))
  (define lower-subset-cut (range-lower-cut subset-range))
  (define upper-subset-cut (range-upper-cut subset-range))
  (define lower-boundary
    (vector-binary-search vec (λ (range) (range-compare-to-cut range lower-subset-cut))))
  (define upper-boundary
    (vector-binary-search vec (λ (range) (range-compare-to-cut range upper-subset-cut))))
  (define start
    (match lower-boundary
      [(exact-match i) i]
      [(no-match pos) pos]))
  (define end
    (match upper-boundary
      [(exact-match i) (add1 i)]
      [(no-match pos) pos]))
  (define subvec (make-vector (- end start)))
  (vector-copy! subvec 0 vec start end)
  (when (exact-match? lower-boundary)
    (define modified-range (range-intersection (vector-ref subvec 0) subset-range))
    (vector-set! subvec 0 modified-range))
  (when (exact-match? upper-boundary)
    (define index-in-subvec (sub1 (vector-length subvec)))
    (define modified-range (range-intersection (vector-ref subvec index-in-subvec) subset-range))
    (vector-set! subvec index-in-subvec modified-range))
  (constructor:range-set (unsafe-vector*->immutable-vector! subvec)))


(module+ test

  (test-case "range-set-size"
    (define ranges (range-set (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (check-equal? (range-set-size ranges) 3))

  (test-case "range-set-contains?"
    (define ranges (range-set (singleton-range 1) (closed-range 4 7) (greater-than-range 10)))
    (check-false (range-set-contains? ranges 0))
    (check-true (range-set-contains? ranges 1))
    (check-false (range-set-contains? ranges 2))
    (check-false (range-set-contains? ranges 3))
    (check-true (range-set-contains? ranges 4))
    (check-true (range-set-contains? ranges 5))
    (check-true (range-set-contains? ranges 6))
    (check-true (range-set-contains? ranges 7))
    (check-false (range-set-contains? ranges 8))
    (check-false (range-set-contains? ranges 9))
    (check-false (range-set-contains? ranges 10))
    (check-true (range-set-contains? ranges 11))
    (check-true (range-set-contains? ranges 12)))

  (test-case (name-string range-set-encloses?)
    (define ranges (range-set (singleton-range 1) (closed-range 4 7) (greater-than-range 10)))
    (check-true (range-set-encloses? ranges (singleton-range 1)))
    (check-true (range-set-encloses? ranges (closed-range 4 7)))
    (check-true (range-set-encloses? ranges (greater-than-range 10)))
    (check-true (range-set-encloses? ranges (closed-range 5 6)))
    (check-true (range-set-encloses? ranges (open-range 4 7)))
    (check-true (range-set-encloses? ranges (closed-open-range 4 7)))
    (check-true (range-set-encloses? ranges (open-closed-range 4 7)))
    (check-true (range-set-encloses? ranges (closed-range 15 20)))
    (check-false (range-set-encloses? ranges (unbounded-range)))
    (check-false (range-set-encloses? ranges (closed-range 3 8)))
    (check-false (range-set-encloses? ranges (closed-range 3 6)))
    (check-false (range-set-encloses? ranges (closed-range 6 8)))
    (check-false (range-set-encloses? ranges (less-than-range 1)))
    (check-false (range-set-encloses? ranges (closed-range 2 3)))
    (check-false (range-set-encloses? ranges (closed-range 8 9)))
    (check-false (range-set-encloses? ranges (closed-range 1 7)))
    (check-false (range-set-encloses? ranges (at-least-range 4))))

  (test-case (name-string range-subset)
    (define ranges (range-set (singleton-range 1) (closed-range 4 7) (greater-than-range 10)))
    (check-equal? (range-subset ranges (unbounded-range)) ranges)

    (test-case "non-intersecting subset selecting middle ranges only"
      (define subset-range (closed-range 3 8))
      (define expected (range-set (closed-range 4 7)))
      (check-equal? (range-subset ranges subset-range) expected))

    (test-case "non-intersecting subset selecting lower ranges only"
      (define subset-range (less-than-range 8))
      (define expected (range-set (singleton-range 1) (closed-range 4 7)))
      (check-equal? (range-subset ranges subset-range) expected))

    (test-case "non-intersecting subset selecting upper ranges only"
      (define subset-range (greater-than-range 3))
      (define expected (range-set (closed-range 4 7) (greater-than-range 10)))
      (check-equal? (range-subset ranges subset-range) expected))

    (test-case "intersecting subset selecting middle ranges only"
      (define subset-range (closed-range 3 6))
      (define expected (range-set (closed-range 4 6)))
      (check-equal? (range-subset ranges subset-range) expected))

    (test-case "intersecting subset selecting lower ranges only"
      (define subset-range (less-than-range 6))
      (define expected (range-set (singleton-range 1) (closed-open-range 4 6)))
      (check-equal? (range-subset ranges subset-range) expected))

    (test-case "intersecting subset selecting upper ranges only"
      (define subset-range (greater-than-range 5))
      (define expected (range-set (open-closed-range 5 7) (greater-than-range 10)))
      (check-equal? (range-subset ranges subset-range) expected))))
