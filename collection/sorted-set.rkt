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
         (submod rebellion/collection/private/regular-immutable-sorted-set private-for-rebellion-only)
         rebellion/collection/private/sorted-set-interface
         rebellion/collection/private/sorted-set-builder
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer)


;@----------------------------------------------------------------------------------------------------


(define (sorted-set #:comparator comparator . elements)
  (sequence->sorted-set elements #:comparator comparator))


(define/guard (sequence->sorted-set elements #:comparator comparator)
  ;; We only avoid copying if the input is a regular-immutable-sorted-set? instead of any
  ;; immutable-sorted-set? because the latter includes subset views. A subset view could be a tiny
  ;; portion of a much larger backing set, and there's a soft expectation that copying a sequence into
  ;; an immutable collection retains space linear in the size of the returned collection.
  (guard (and (regular-immutable-sorted-set? elements)
              (equal? (sorted-set-comparator elements) comparator)) then
    elements)
  (transduce elements #:into (into-sorted-set comparator)))


(define (into-sorted-set comparator)
  (make-effectful-fold-reducer
   sorted-set-builder-add
   (λ () (make-sorted-set-builder comparator))
   build-sorted-set
   #:name (name into-sorted-set)))
