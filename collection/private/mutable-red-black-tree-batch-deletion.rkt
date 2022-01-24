#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-rb-subtree-clear! (-> mutable-rb-tree? range? void?)]))


(require racket/sequence
         rebellion/base/range
         rebellion/collection/private/mutable-red-black-tree-base
         rebellion/collection/private/mutable-red-black-tree-deletion
         rebellion/collection/private/mutable-red-black-tree-iteration)


;@----------------------------------------------------------------------------------------------------


(define (mutable-rb-subtree-clear! tree key-range)
  ;; There's definitely a faster algorithm for this than just collecting all the elements into a list
  ;; and removing them one at a time. But this works fine for now, and it has the advantages of being
  ;; simple, easy to implement, and obviously correct.
  (define keys (sequence->list (in-mutable-rb-subtree-keys tree key-range)))
  ;; It's important that we don't remove keys and iterate over the tree at the same time, as tree
  ;; rotations could invalidate assumptions the tree iteration code is making. By collecting the keys
  ;; into a list and then iterating over the list, we ensure everything is in a consistent state.
  (for ([key (in-list keys)])
    (mutable-rb-tree-remove! tree key)))
