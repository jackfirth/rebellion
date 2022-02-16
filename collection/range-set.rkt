#lang racket/base


(require racket/contract/base)


(provide
 for/range-set
 for*/range-set
 (contract-out
  [range-set? predicate/c]
  [range-set (->* () (#:comparator comparator?) #:rest (listof nonempty-range?) range-set?)]
  [range-set-size (-> range-set? natural?)]
  [range-set-comparator (-> range-set? comparator?)]
  [sequence->range-set (-> (sequence/c nonempty-range?) #:comparator comparator? range-set?)]
  [into-range-set (-> comparator? (reducer/c nonempty-range? range-set?))]
  [in-range-set (-> range-set? (sequence/c nonempty-range?))]
  [empty-range-set? predicate/c]
  [nonempty-range-set? predicate/c]
  [range-set-contains? (-> range-set? any/c boolean?)]
  [range-set-encloses? (-> range-set? range? boolean?)]
  [range-set-encloses-all? (-> range-set? (or/c range-set? (sequence/c range?)) boolean?)]
  [range-subset (-> range-set? range? range-set?)]))


(require (for-syntax racket/base
                     rebellion/private/for-body)
         (only-in racket/list empty? first)
         racket/match
         racket/math
         racket/sequence
         racket/stream
         racket/struct
         (only-in racket/unsafe/ops unsafe-vector*->immutable-vector!)
         racket/vector
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/entry
         rebellion/collection/private/vector-binary-search
         rebellion/collection/sorted-map
         rebellion/collection/vector/builder
         rebellion/private/cut
         rebellion/private/guarded-block
         rebellion/private/precondition
         rebellion/private/static-name
         rebellion/streaming/reducer
         (submod rebellion/streaming/reducer private-for-rebellion-only)
         rebellion/streaming/transducer
         rebellion/type/tuple
         syntax/parse/define)


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


;@----------------------------------------------------------------------------------------------------
;; Data definition


(struct range-set (endpoints comparator)
  #:omit-define-syntaxes
  #:constructor-name constructor:range-set

  #:property prop:sequence (λ (this) (in-range-set this))

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer
      (λ (this) 'range-set)
      (λ (this) this)))]

  #:methods gen:equal+hash

  [(define (equal-proc this other recur)
     (recur (range-set-endpoints this) (range-set-endpoints other)))

   (define (hash-proc this recur)
     (recur (range-set-endpoints this)))

   (define hash2-proc hash-proc)])


(define (range-set #:comparator [comparator #false] . ranges)
  (check-precondition
   (or comparator (not (empty? ranges)))
   (name range-set)
   "cannot construct an empty range set without a comparator")
  (let ([comparator (or comparator (range-comparator (first ranges)))])
    (sequence->range-set ranges #:comparator comparator)))


(define (sequence->range-set ranges #:comparator comparator)
  (transduce ranges #:into (into-range-set comparator)))


(define (empty-range-set? v)
  (and (range-set? v) (sorted-map-empty? (range-set-endpoints v))))


(define (nonempty-range-set? v)
  (and (range-set? v) (not (sorted-map-empty? (range-set-endpoints v)))))


(define (in-range-set ranges)
  (define comparator (range-set-comparator ranges))
  (for/stream ([e (in-sorted-map (range-set-endpoints ranges))])
    (match-define (entry lower upper) e)
    (range-from-cuts lower upper #:comparator comparator)))


(struct range-set-builder ([range-vector-builder #:mutable] comparator))


(define (make-range-set-builder comparator)
  (range-set-builder (make-vector-builder) comparator))


(define (range-set-builder-add-range builder range)
  (define vector-builder (vector-builder-add (range-set-builder-range-vector-builder builder) range))
  (set-range-set-builder-range-vector-builder! builder vector-builder)
  builder)


(define (build-range-set builder)
  (define ranges (build-vector (range-set-builder-range-vector-builder builder)))
  (define comparator (range-set-builder-comparator builder))
  (check-ranges-use-comparator #:who (name build-range-set) ranges comparator)
  (define sorted-ranges (vector-sort ranges range<?))
  (check-ranges-disjoint #:who (name build-range-set) sorted-ranges)
  (define coalesced-ranges (vector-merge-adjacent sorted-ranges range-connected? range-span))
  (define endpoints
    (for/sorted-map #:key-comparator (cut<=> comparator) ([range (in-vector coalesced-ranges)])
      (entry (range-lower-cut range) (range-upper-cut range))))
  (constructor:range-set endpoints comparator))


(define (check-ranges-use-comparator #:who who ranges comparator)
  (for ([range (in-vector ranges)])
    (check-precondition
     (equal? (range-comparator range) comparator)
     who
     "not all ranges use the same comparator"
     "range" range
     "range comparator" (range-comparator range)
     "expected comparator" comparator)))


(define (range<? range other-range)
  (equal? (compare range<=> range other-range) lesser))


(define (check-ranges-disjoint #:who who ranges)
  (unless (zero? (vector-length ranges))
    (for ([range (in-vector ranges)]
          [next-range (in-vector ranges 1)])
      (when (range-overlaps? range next-range)
        (raise-arguments-error
         who
         "overlapping ranges not allowed"
         "range" range
         "next range" next-range)))))


(define (into-range-set comparator)

  (define (start)
    (make-range-set-builder comparator))
  
  (make-effectful-fold-reducer
   range-set-builder-add-range start build-range-set #:name (name into-range-set)))


(define-syntax-parse-rule (for/range-set #:comparator comparator clauses body)
  #:declare comparator (expr/c #'comparator?)
  #:declare body (for-body this-syntax)
  #:with context this-syntax
  (for/reducer/derived context (into-range-set comparator.c) clauses (~@ . body)))


(define-syntax-parse-rule (for*/range-set #:comparator comparator clauses body)
  #:declare comparator (expr/c #'comparator?)
  #:declare body (for-body this-syntax)
  #:with context this-syntax
  (for*/reducer/derived context (into-range-set comparator.c) clauses (~@ . body)))


(module+ test

  (test-case "range sets"

    (test-case "single range"
      (define actual (range-set (closed-range 1 4)))
      (check-equal? actual (range-set (closed-range 1 4)))
      (check-not-equal? actual (range-set (closed-range 5 8)))
      (check-not-equal? actual (range-set #:comparator real<=>)))

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
    (check-equal? (sequence->range-set ranges #:comparator real<=>) expected))

  (test-case "into-range-set"
    (define ranges (list (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (define expected (range-set (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (check-equal? (transduce ranges #:into (into-range-set real<=>)) expected)))


;@----------------------------------------------------------------------------------------------------
;; Queries


(define (range-set-binary-search ranges value)
  #false)


(define (range-set-size ranges)
  (sorted-map-size (range-set-endpoints ranges)))


(define/guard (range-set-get-nearest-range ranges cut)
  (define endpoints (range-set-endpoints ranges))
  (guard-match (present (entry lower-endpoint upper-endpoint))
    (sorted-map-entry-at-most endpoints cut)
    else
    absent)
  (present
   (range-from-cuts lower-endpoint upper-endpoint #:comparator (range-set-comparator ranges))))


(define/guard (range-set-contains? ranges value)
  (match (range-set-get-nearest-range ranges (middle-cut value))
    [(== absent) #false]
    [(present nearest-range) (range-contains? nearest-range value)]))


(define/guard (range-set-encloses? ranges other-range)
  (match (range-set-get-nearest-range ranges (range-lower-cut other-range))
    [(== absent) #false]
    [(present nearest-range) (range-encloses? nearest-range other-range)]))


(define (range-set-encloses-all? ranges other-ranges)
  (for/and ([range other-ranges])
    (range-set-encloses? ranges range)))


(define/guard (range-subset ranges subset-range)
  (define cut-comparator (cut<=> (range-comparator subset-range)))
  (define lower-subset-cut (range-lower-cut subset-range))
  (define upper-subset-cut (range-upper-cut subset-range))
  (define subset-endpoint-range
    (closed-range lower-subset-cut upper-subset-cut #:comparator cut-comparator))

  (define endpoints-submap (sorted-submap (range-set-endpoints ranges) subset-endpoint-range))

  (define endpoints-submap-with-left-end-corrected
    (guarded-block
     (guard-match (present (entry leftmost-range-lower-cut leftmost-range-upper-cut))
       (sorted-map-entry-at-most (range-set-endpoints ranges) lower-subset-cut)
       else
       endpoints-submap)
     (guard (compare-infix cut-comparator leftmost-range-upper-cut > lower-subset-cut) else
       endpoints-submap)
     (define corrected-lower-range
       (range-from-cuts lower-subset-cut leftmost-range-upper-cut #:comparator cut-comparator))
     (guard (empty-range? corrected-lower-range) then
       endpoints-submap)
     (sorted-map-put endpoints-submap lower-subset-cut leftmost-range-upper-cut)))

  (define endpoints-submap-with-right-end-corrected
    (guarded-block
     (guard-match (present (entry rightmost-range-lower-cut rightmost-range-upper-cut))
       (sorted-map-greatest-entry endpoints-submap-with-left-end-corrected)
       else
       endpoints-submap-with-left-end-corrected)
     (define corrected-upper-cut
       (comparator-min cut-comparator rightmost-range-upper-cut upper-subset-cut))
     (define corrected-rightmost-range
       (range-from-cuts rightmost-range-lower-cut corrected-upper-cut #:comparator cut-comparator))
     (guard (empty-range? corrected-rightmost-range) then
       (sorted-map-remove endpoints-submap-with-left-end-corrected rightmost-range-lower-cut))
     (sorted-map-put
      endpoints-submap-with-left-end-corrected rightmost-range-lower-cut corrected-upper-cut)))

  (constructor:range-set endpoints-submap-with-right-end-corrected (range-set-comparator ranges)))


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

  (test-case (name-string range-set-encloses-all?)
    (define ranges (range-set (singleton-range 1) (closed-range 4 7) (greater-than-range 10)))
    (check-true (range-set-encloses-all? ranges ranges))
    (check-true (range-set-encloses-all? ranges '()))
    (check-true (range-set-encloses-all? ranges (list (singleton-range 1))))
    (check-true (range-set-encloses-all? ranges (list (closed-range 4 7))))
    (check-true (range-set-encloses-all? ranges (list (greater-than-range 10))))
    (check-false (range-set-encloses-all? ranges (list (unbounded-range))))
    (check-false (range-set-encloses-all? ranges (list (closed-range 0 1))))
    (check-false (range-set-encloses-all? ranges (list (closed-range 1 2))))
    (check-false (range-set-encloses-all? ranges (list (singleton-range 1) (singleton-range 2)))))

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
