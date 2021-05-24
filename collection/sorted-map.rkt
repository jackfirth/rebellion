#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-map? predicate/c]
  [mutable-sorted-map? predicate/c]
  [immutable-sorted-map? predicate/c]
  [make-mutable-sorted-map (-> comparator? mutable-sorted-map?)]
  [sorted-map-put! (-> mutable-sorted-map? any/c any/c void?)]
  [sorted-map-contains-key? (-> sorted-map? any/c boolean?)]
  [sorted-map-get-or-absent (-> sorted-map? any/c option?)]
  [sorted-map-remove! (-> mutable-sorted-map? any/c void?)]))


(require racket/block
         racket/match
         racket/sequence
         racket/stream
         rebellion/base/comparator
         rebellion/base/option
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct sorted-map ())


(struct mutable-sorted-map sorted-map (red-black-tree)
  #:constructor-name constructor:mutable-sorted-map)


(define (make-mutable-sorted-map comparator)
  (constructor:mutable-sorted-map comparator))


(struct immutable-sorted-map ())


(define (sorted-map-put! map key value)
  (void))


(define (sorted-map-contains-key? map key)
  #false)


(define (sorted-map-get-or-absent map key)
  absent)


(define (sorted-map-remove! map key)
  (void))
