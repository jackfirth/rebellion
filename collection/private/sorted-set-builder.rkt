#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-set-builder? (-> any/c boolean?)]
  [sorted-set-builder-add (-> sorted-set-builder? any/c any/c ... sorted-set-builder?)]
  [sorted-set-builder-add-all (-> sorted-set-builder? (sequence/c any/c) sorted-set-builder?)]
  [make-sorted-set-builder (-> comparator? sorted-set-builder?)]
  [build-sorted-set (-> sorted-set-builder? immutable-sorted-set?)]))


(require racket/sequence
         racket/vector
         rebellion/base/comparator
         rebellion/collection/vector
         rebellion/collection/vector/builder
         (submod rebellion/collection/private/persistent-sorted-set private-for-rebellion-only)
         (submod rebellion/collection/private/regular-immutable-sorted-set private-for-rebellion-only)
         rebellion/collection/private/sorted-set-interface
         rebellion/streaming/transducer
         guard)


;@----------------------------------------------------------------------------------------------------


(struct sorted-set-builder
  (comparator vector-builder)
  #:guard (struct-guard/c comparator? vector-builder?)
  #:omit-define-syntaxes
  #:constructor-name constructor:sorted-set-builder)


(define (make-sorted-set-builder comparator)
  (constructor:sorted-set-builder comparator (make-vector-builder)))


(define (sorted-set-builder-add builder . elements)
  (sorted-set-builder-add-all builder elements))


(define (sorted-set-builder-add-all builder elements)
  (define modified (vector-builder-add-all (sorted-set-builder-vector-builder builder) elements))
  (constructor:sorted-set-builder (sorted-set-builder-comparator builder) modified))


(define/guard (build-sorted-set builder)
  (define element<=> (sorted-set-builder-comparator builder))
  (define mutable-elements (build-mutable-vector (sorted-set-builder-vector-builder builder)))
  (guard (positive? (vector-length mutable-elements)) #:else
    (empty-sorted-set element<=>))
  
  (define (< x y)
    (equal? (compare element<=> x y) lesser))
  
  (vector-sort! mutable-elements <)
  (define deduplicated
    (transduce mutable-elements
               (deduplicating-consecutive)
               #:into (into-vector #:size (vector-length mutable-elements))))
  (make-regular-immutable-sorted-set deduplicated element<=>))
