#lang racket/base


(require racket/contract/base)


(provide
 (all-from-out rebellion/collection/private/mutable-sorted-set)
 (all-from-out rebellion/collection/private/sorted-set-builder)
 (all-from-out rebellion/collection/private/sorted-set-interface)
 (contract-out
  [sorted-set (-> #:comparator comparator? any/c ... immutable-sorted-set?)]
  [sequence->sorted-set (-> (sequence/c any/c) #:comparator comparator? immutable-sorted-set?)]
  [into-sorted-set (-> comparator? (reducer/c any/c immutable-sorted-set?))]))


(require racket/sequence
         rebellion/base/comparator
         rebellion/collection/private/mutable-sorted-set
         rebellion/collection/private/sorted-set-interface
         rebellion/collection/private/sorted-set-builder
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer)


;@----------------------------------------------------------------------------------------------------


(define (sorted-set #:comparator comparator . elements)
  (sequence->sorted-set elements #:comparator comparator))


(define (sequence->sorted-set elements #:comparator comparator)
  (transduce elements #:into (into-sorted-set comparator)))


(define (into-sorted-set comparator)
  (make-effectful-fold-reducer
   sorted-set-builder-add
   (λ () (make-sorted-set-builder comparator))
   build-sorted-set
   #:name (name into-sorted-set)))
