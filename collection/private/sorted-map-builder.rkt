#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-map-builder? predicate/c]
  [sorted-map-builder-put (-> sorted-map-builder? any/c any/c sorted-map-builder?)]
  [sorted-map-builder-put-all (-> sorted-map-builder? (sequence/c entry?) sorted-map-builder?)]
  [make-sorted-map-builder (-> comparator? sorted-map-builder?)]
  [build-sorted-map (-> sorted-map-builder? immutable-sorted-map?)]))


(require racket/match
         racket/sequence
         racket/unsafe/ops
         racket/vector
         rebellion/base/comparator
         rebellion/collection/entry
         rebellion/collection/vector
         rebellion/collection/vector/builder
         (submod rebellion/collection/private/persistent-sorted-map private-for-rebellion-only)
         (submod rebellion/collection/private/regular-immutable-sorted-map private-for-rebellion-only)
         rebellion/collection/private/sorted-map-interface
         rebellion/streaming/transducer
         guard
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(struct sorted-map-builder
  (key-comparator entry-vector-builder)
  #:omit-define-syntaxes
  #:constructor-name constructor:sorted-map-builder)


(define (make-sorted-map-builder key-comparator)
  (constructor:sorted-map-builder key-comparator (make-vector-builder)))


(define (sorted-map-builder-put builder key value)
  (vector-builder-add (sorted-map-builder-entry-vector-builder builder) (entry key value))
  builder)


(define (sorted-map-builder-put-all builder entries)
  (vector-builder-add-all (sorted-map-builder-entry-vector-builder builder) entries)
  builder)


(define/guard (build-sorted-map builder)
  (define key<=> (sorted-map-builder-key-comparator builder))
  (define mutable-entries (build-mutable-vector (sorted-map-builder-entry-vector-builder builder)))

  (guard (not (vector-empty? mutable-entries)) #:else
    (empty-sorted-map key<=>))

  (define (entry< e1 e2)
    (compare-infix key<=> (entry-key e1) < (entry-key e2)))

  (vector-sort! mutable-entries entry<)

  (define value-vector (make-vector (vector-length mutable-entries)))

  (for/fold ([previous #false] #:result (void))
            ([i (in-range (vector-length mutable-entries))])
    (define e (vector-ref mutable-entries i))
    (match-define (entry key value) e)
    (when (and previous (compare-infix key<=> (entry-key previous) == key))
      (raise-arguments-error
       (name build-sorted-map)
       "multiple values for the same key are not allowed"
       "key" key
       "value1" (entry-value previous)
       "value2" value))
    (vector-set! mutable-entries i key)
    (vector-set! value-vector i value)
    e)

  (constructor:regular-immutable-sorted-map
   (unsafe-vector*->immutable-vector! mutable-entries)
   (unsafe-vector*->immutable-vector! value-vector)
   key<=>))
