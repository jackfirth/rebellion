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
         racket/vector
         rebellion/base/range
         rebellion/collection/vector/builder
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record)


;@----------------------------------------------------------------------------------------------------


(define-record-type range-set (sorted-range-vector)
  #:omit-root-binding)


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
  (check-ranges-use-same-comparator ranges)
  (define sorted-ranges (vector-sort ranges range<?))
  (check-ranges-disjoint sorted-ranges)
  (constructor:range-set #:sorted-range-vector (vector->immutable-vector sorted-ranges)))


(define (check-ranges-use-same-comparator ranges)
  (void))


(define (range<? range other-range)
  (error 'range<? "not implemented"))


(define (check-ranges-disjoint ranges)
  (void))


(define into-range-set
  (make-effectful-fold-reducer
   range-set-builder-add-range make-range-set-builder build-range-set #:name (name into-range-set)))


(define empty-range-set (range-set))


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
