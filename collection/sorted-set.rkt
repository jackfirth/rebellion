#lang racket/base


(require racket/contract/base)


;; TODO: replace this with recontract out from component modules (or maybe just all-from-out)
(provide
 (contract-out
  [sorted-set? predicate/c]
  [immutable-sorted-set? predicate/c]
  [mutable-sorted-set? predicate/c]
  [make-mutable-sorted-set (->* (#:comparator comparator?) ((sequence/c any/c)) mutable-sorted-set?)]
  [sorted-set (-> #:comparator comparator? any/c ... immutable-sorted-set?)]
  [empty-sorted-set (-> comparator? immutable-sorted-set?)]
  [sequence->sorted-set (-> (sequence/c any/c) #:comparator comparator? immutable-sorted-set?)]
  [in-sorted-set (-> sorted-set? (sequence/c any/c))]
  [into-sorted-set (-> comparator? (reducer/c any/c immutable-sorted-set?))]
  [sorted-set-size (-> sorted-set? natural?)]
  [sorted-set-comparator (-> sorted-set? comparator?)]
  [sorted-set-contains? (-> sorted-set? any/c boolean?)]
  [sorted-set-contains-all? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-contains-any? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-contains-none? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-least-element (-> sorted-set? option?)]
  [sorted-set-greatest-element (-> sorted-set? option?)]
  [sorted-set-element-less-than (-> sorted-set? any/c option?)]
  [sorted-set-element-at-most (-> sorted-set? any/c option?)]
  [sorted-set-element-greater-than (-> sorted-set? any/c option?)]
  [sorted-set-element-at-least (-> sorted-set? any/c option?)]
  [sorted-set-add (-> immutable-sorted-set? any/c immutable-sorted-set?)]
  [sorted-set-add! (-> mutable-sorted-set? any/c void?)]
  [sorted-set-add-all (-> immutable-sorted-set? (sequence/c any/c) immutable-sorted-set?)]
  [sorted-set-add-all! (-> mutable-sorted-set? (sequence/c any/c) void?)]
  [sorted-set-remove (-> immutable-sorted-set? any/c immutable-sorted-set?)]
  [sorted-set-remove! (-> mutable-sorted-set? any/c void?)]
  [sorted-set-remove-all (-> immutable-sorted-set? (sequence/c any/c) immutable-sorted-set?)]
  [sorted-set-remove-all! (-> mutable-sorted-set? (sequence/c any/c) void?)]
  [sorted-set-clear! (-> mutable-sorted-set? void?)]
  [sorted-subset (-> sorted-set? range? sorted-set?)]
  [sorted-set-reverse (-> sorted-set? sorted-set?)]
  [sorted-set-builder? predicate/c]
  [sorted-set-builder-add (-> sorted-set-builder? any/c any/c ... sorted-set-builder?)]
  [sorted-set-builder-add-all (-> sorted-set-builder? (sequence/c any/c) sorted-set-builder?)]
  [make-sorted-set-builder (-> comparator? sorted-set-builder?)]
  [build-sorted-set (-> sorted-set-builder? immutable-sorted-set?)]))


(require racket/math
         racket/sequence
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/private/mutable-sorted-set
         rebellion/collection/private/persistent-sorted-set
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
