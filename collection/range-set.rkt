#lang racket/base


(require racket/contract/base
         rebellion/collection/private/endpoint-map-range-set
         rebellion/collection/private/range-set-interface)


(provide
 for/range-set
 for*/range-set
 (recontract-out
  range-set?
  immutable-range-set?
  mutable-range-set?
  range-set
  make-mutable-range-set
  sequence->range-set
  into-range-set
  in-range-set
  range-set-comparator
  range-set-empty?
  range-set-size
  range-set-contains?
  range-set-contains-all?
  range-set-encloses?
  range-set-encloses-all?
  range-set-intersects?
  range-set-range-containing
  range-set-range-containing-or-absent
  range-set-span
  range-set-span-or-absent
  range-set-add
  range-set-add!
  range-set-add-all
  range-set-add-all!
  range-set-remove
  range-set-remove!
  range-set-remove-all
  range-set-remove-all!
  range-set-clear!
  range-subset)
 (contract-out
  [empty-range-set? predicate/c]
  [nonempty-range-set? predicate/c]))


(module+ test
  (require (submod "..")
           racket/sequence
           rackunit
           rebellion/base/comparator
           rebellion/base/range
           rebellion/private/static-name
           rebellion/private/todo
           rebellion/streaming/transducer))


;@----------------------------------------------------------------------------------------------------
;; Data definition

(define (empty-range-set? v)
  (and (range-set? v) (range-set-empty? v)))


(define (nonempty-range-set? v)
  (and (range-set? v) (not (range-set-empty? v))))


(module+ test

  (test-case "range set construction"

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


  (test-case (name-string in-range-set)
    (define expected (list (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))

    (test-case "immutable range sets"
      (define ranges (range-set (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
      (check-equal? (sequence->list (in-range-set ranges)) expected))
    
    (test-case "mutable range sets"
      (define ranges
        (make-mutable-range-set
         (list (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)) #:comparator real<=>))
      (check-equal? (sequence->list (in-range-set ranges)) expected)))
  

  (test-case (name-string for/range-set)
    (define expected (range-set (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (define actual
      (for/range-set #:comparator real<=>
        ([lower (in-list (list 2 6 10))]
         [upper (in-list (list 4 8 12))])
        (closed-range lower upper)))
    (check-equal? actual expected))
  

  (test-case (name-string for*/range-set)
    (define range-lists
      (list (list (closed-range 2 4) (closed-range 6 8) (closed-range 10 12))
            (list (closed-range 20 40) (closed-range 60 80) (closed-range 100 120))
            (list (closed-range 200 400) (closed-range 600 800) (closed-range 1000 1200))))
    (define expected
      (range-set
       (closed-range 2 4)
       (closed-range 6 8)
       (closed-range 10 12)
       (closed-range 20 40)
       (closed-range 60 80)
       (closed-range 100 120)
       (closed-range 200 400)
       (closed-range 600 800)
       (closed-range 1000 1200)))
    (define actual
      (for*/range-set #:comparator real<=>
        ([range-list (in-list range-lists)]
         [r (in-list range-list)])
        r))
    (check-equal? actual expected))
  

  (test-case "into-range-set"
    (define ranges (list (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (define expected (range-set (closed-range 2 4) (closed-range 6 8) (closed-range 10 12)))
    (check-equal? (transduce ranges #:into (into-range-set real<=>)) expected))


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
      (check-equal? (range-subset ranges subset-range) expected)))


  (test-case (name-string range-set-add)

    (test-case "empty case"
      (define ranges (range-set #:comparator real<=>))

      (test-case "inserting bounded range"
        (define actual (range-set-add ranges (closed-range 4 8)))
        (define expected (range-set (closed-range 4 8)))
        (check-equal? actual expected))

      (test-case "inserting unbounded range"
        (define actual (range-set-add ranges (unbounded-range)))
        (define expected (range-set (unbounded-range)))
        (check-equal? actual expected))

      (test-case "inserting bounded-below range"
        (define actual (range-set-add ranges (greater-than-range 6)))
        (define expected (range-set (greater-than-range 6)))
        (check-equal? actual expected))

      (test-case "inserting bounded-above range"
        (define actual (range-set-add ranges (less-than-range 5)))
        (define expected (range-set (less-than-range 5)))
        (check-equal? actual expected)))


    (test-case "singleton case"
      (define ranges (range-set (closed-open-range 5 10)))

      (test-case "insert range before"
        (define actual (range-set-add ranges (closed-open-range 2 4)))
        (define expected (range-set (closed-open-range 2 4) (closed-open-range 5 10)))
        (check-equal? actual expected))

      (test-case "insert adjacent range before"
        (define actual (range-set-add ranges (closed-open-range 2 5)))
        (define expected (range-set (closed-open-range 2 10)))
        (check-equal? actual expected))

      (test-case "insert overlapping range before"
        (define actual (range-set-add ranges (closed-open-range 2 8)))
        (define expected (range-set (closed-open-range 2 10)))
        (check-equal? actual expected))

      (test-case "insert enclosing range before"
        (define actual (range-set-add ranges (closed-open-range 2 10)))
        (define expected (range-set (closed-open-range 2 10)))
        (check-equal? actual expected))

      (test-case "insert range after"
        (define actual (range-set-add ranges (closed-open-range 15 20)))
        (define expected (range-set (closed-open-range 5 10) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert adjacent range after"
        (define actual (range-set-add ranges (closed-open-range 10 20)))
        (define expected (range-set (closed-open-range 5 20)))
        (check-equal? actual expected))

      (test-case "insert overlapping range after"
        (define actual (range-set-add ranges (closed-open-range 8 20)))
        (define expected (range-set (closed-open-range 5 20)))
        (check-equal? actual expected))

      (test-case "insert enclosing range after"
        (define actual (range-set-add ranges (closed-open-range 5 20)))
        (define expected (range-set (closed-open-range 5 20)))
        (check-equal? actual expected)))

    (test-case "two range case"
      (define ranges (range-set (closed-open-range 5 10) (closed-open-range 15 20)))

      (test-case "insert range before"
        (define actual (range-set-add ranges (closed-open-range 2 4)))
        (define expected
          (range-set (closed-open-range 2 4) (closed-open-range 5 10) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert adjacent range before"
        (define actual (range-set-add ranges (closed-open-range 2 5)))
        (define expected (range-set (closed-open-range 2 10) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert overlapping range before"
        (define actual (range-set-add ranges (closed-open-range 2 8)))
        (define expected (range-set (closed-open-range 2 10) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert enclosing range before"
        (define actual (range-set-add ranges (closed-open-range 2 10)))
        (define expected (range-set (closed-open-range 2 10) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert range after"
        (define actual (range-set-add ranges (closed-open-range 22 25)))
        (define expected
          (range-set (closed-open-range 5 10) (closed-open-range 15 20) (closed-open-range 22 25)))
        (check-equal? actual expected))

      (test-case "insert adjacent range after"
        (define actual (range-set-add ranges (closed-open-range 20 25)))
        (define expected
          (range-set (closed-open-range 5 10) (closed-open-range 15 25)))
        (check-equal? actual expected))

      (test-case "insert overlapping range after"
        (define actual (range-set-add ranges (closed-open-range 18 25)))
        (define expected
          (range-set (closed-open-range 5 10) (closed-open-range 15 25)))
        (check-equal? actual expected))

      (test-case "insert enclosing range after"
        (define actual (range-set-add ranges (closed-open-range 15 25)))
        (define expected
          (range-set (closed-open-range 5 10) (closed-open-range 15 25)))
        (check-equal? actual expected))

      (test-case "insert range between"
        (define actual (range-set-add ranges (closed-open-range 12 14)))
        (define expected
          (range-set (closed-open-range 5 10) (closed-open-range 12 14) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert left-adjacent range between"
        (define actual (range-set-add ranges (closed-open-range 10 14)))
        (define expected (range-set (closed-open-range 5 14) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert right-adjacent range between"
        (define actual (range-set-add ranges (closed-open-range 12 15)))
        (define expected (range-set (closed-open-range 5 10) (closed-open-range 12 20)))
        (check-equal? actual expected))

      (test-case "insert left-overlapping range between"
        (define actual (range-set-add ranges (closed-open-range 8 14)))
        (define expected (range-set (closed-open-range 5 14) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert right-overlapping range between"
        (define actual (range-set-add ranges (closed-open-range 12 18)))
        (define expected (range-set (closed-open-range 5 10) (closed-open-range 12 20)))
        (check-equal? actual expected))

      (test-case "insert left-enclosing range between"
        (define actual (range-set-add ranges (closed-open-range 5 14)))
        (define expected (range-set (closed-open-range 5 14) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "insert right-enclosing range between"
        (define actual (range-set-add ranges (closed-open-range 12 20)))
        (define expected (range-set (closed-open-range 5 10) (closed-open-range 12 20)))
        (check-equal? actual expected))))

  (test-case (name-string range-set-remove)

    (test-case "empty case"
      (define ranges (range-set #:comparator real<=>))
      (check-equal? (range-set-remove ranges (closed-range 5 10)) ranges)
      (check-equal? (range-set-remove ranges (open-range 5 10)) ranges)
      (check-equal? (range-set-remove ranges (closed-open-range 5 10)) ranges)
      (check-equal? (range-set-remove ranges (open-closed-range 5 10)) ranges)
      (check-equal? (range-set-remove ranges (greater-than-range 5)) ranges)
      (check-equal? (range-set-remove ranges (at-least-range 10)) ranges)
      (check-equal? (range-set-remove ranges (less-than-range 5)) ranges)
      (check-equal? (range-set-remove ranges (at-most-range 5)) ranges)
      (check-equal? (range-set-remove ranges (unbounded-range)) ranges))

    (test-case "singleton case"
      (define ranges (range-set (closed-open-range 5 10)))

      (test-case "remove range before"
        (check-equal? (range-set-remove ranges (closed-range 2 3)) ranges))

      (test-case "remove adjacent range before"
        (check-equal? (range-set-remove ranges (closed-open-range 2 5)) ranges))

      (test-case "remove overlapping range before"
        (define actual (range-set-remove ranges (closed-range 2 7)))
        (define expected (range-set (open-range 7 10)))
        (check-equal? actual expected))

      (test-case "remove enclosing range before"
        (define actual (range-set-remove ranges (closed-open-range 2 10)))
        (define expected (range-set #:comparator real<=>))
        (check-equal? actual expected))

      (test-case "remove range after"
        (check-equal? (range-set-remove ranges (closed-range 12 14)) ranges))

      (test-case "remove adjacent range after"
        (check-equal? (range-set-remove ranges (closed-range 10 14)) ranges))

      (test-case "remove overlapping range after"
        (define actual (range-set-remove ranges (closed-range 7 12)))
        (define expected (range-set (closed-open-range 5 7)))
        (check-equal? actual expected))

      (test-case "remove enclosing range after"
        (define actual (range-set-remove ranges (closed-range 5 12)))
        (define expected (range-set #:comparator real<=>))
        (check-equal? actual expected))

      (test-case "remove enclosed range"
        (define actual (range-set-remove ranges (closed-range 7 9)))
        (define expected (range-set (closed-open-range 5 7) (open-range 9 10)))
        (check-equal? actual expected)))

    (test-case "two-range case"
      (define ranges (range-set (closed-open-range 5 10) (closed-open-range 15 20)))

      (test-case "remove range before"
        (check-equal? (range-set-remove ranges (closed-range 2 3)) ranges))

      (test-case "remove adjacent range before"
        (check-equal? (range-set-remove ranges (open-range 2 5)) ranges))

      (test-case "remove overlapping range before"
        (define actual (range-set-remove ranges (closed-range 2 7)))
        (define expected (range-set (open-range 7 10) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "remove enclosing range before"
        (define actual (range-set-remove ranges (closed-open-range 2 10)))
        (define expected (range-set (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "remove left-enclosed range"
        (define actual (range-set-remove ranges (closed-range 7 8)))
        (define expected
          (range-set (closed-open-range 5 7) (open-range 8 10) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "remove range after"
        (check-equal? (range-set-remove ranges (closed-range 25 28)) ranges))

      (test-case "remove adjacent range after"
        (check-equal? (range-set-remove ranges (closed-range 20 28)) ranges))

      (test-case "remove overlapping range after"
        (define actual (range-set-remove ranges (closed-range 18 28)))
        (define expected (range-set (closed-open-range 5 10) (closed-open-range 15 18)))
        (check-equal? actual expected))
      
      (test-case "remove enclosing range after"
        (define actual (range-set-remove ranges (closed-range 15 28)))
        (define expected (range-set (closed-open-range 5 10)))
        (check-equal? actual expected))

      (test-case "remove right-enclosed range"
        (define actual (range-set-remove ranges (open-range 17 18)))
        (define expected
          (range-set (closed-open-range 5 10) (closed-range 15 17) (closed-open-range 18 20)))
        (check-equal? actual expected))

      (test-case "remove range between"
        (check-equal? (range-set-remove ranges (closed-range 12 14)) ranges))

      (test-case "remove left-adjacent range between"
        (check-equal? (range-set-remove ranges (closed-range 10 14)) ranges))

      (test-case "remove right-adjacent range between"
        (check-equal? (range-set-remove ranges (closed-open-range 12 15)) ranges))

      (test-case "remove left and right-adjacent range between"
        (check-equal? (range-set-remove ranges (closed-open-range 10 15)) ranges))

      (test-case "remove left-overlapping range between"
        (define actual (range-set-remove ranges (closed-range 8 14)))
        (define expected (range-set (closed-open-range 5 8) (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "remove right-overlapping range between"
        (define actual (range-set-remove ranges (closed-range 12 17)))
        (define expected (range-set (closed-open-range 5 10) (open-range 17 20)))
        (check-equal? actual expected))

      (test-case "remove left and right-overlapping range between"
        (define actual (range-set-remove ranges (closed-range 8 17)))
        (define expected (range-set (closed-open-range 5 8) (open-range 17 20)))
        (check-equal? actual expected))

      (test-case "remove left-enclosing range between"
        (define actual (range-set-remove ranges (closed-range 5 14)))
        (define expected (range-set (closed-open-range 15 20)))
        (check-equal? actual expected))

      (test-case "remove right-enclosing range between"
        (define actual (range-set-remove ranges (closed-open-range 12 20)))
        (define expected (range-set (closed-open-range 5 10)))
        (check-equal? actual expected))

      (test-case "remove left and right-enclosing range between"
        (define actual (range-set-remove ranges (closed-open-range 5 20)))
        (define expected (range-set #:comparator real<=>))
        (check-equal? actual expected))

      (test-case "remove span enclosing range"
        (define actual (range-set-remove ranges (closed-range 2 25)))
        (define expected (range-set #:comparator real<=>))
        (check-equal? actual expected)))))
