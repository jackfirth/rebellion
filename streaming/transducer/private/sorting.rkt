#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [sorting
   (->* () (comparator? #:key (-> any/c any/c) #:descending? boolean?)
        transducer?)]))

(require guard
         racket/bool
         racket/sequence
         rebellion/base/comparator
         rebellion/base/impossible-function
         rebellion/base/option
         rebellion/base/variant
         rebellion/collection/list
         rebellion/streaming/transducer/base
         rebellion/type/record
         rebellion/type/singleton
         rebellion/type/wrapper)

;@------------------------------------------------------------------------------

;; The implementation of sorting transducers is lazily efficient. If only the
;; k smallest elements of a sequence of n elements are demanded, then the
;; remaining elements are left unsorted. This is faster than the O(n log n) cost
;; of a full sort, but I'm unsure what the exact running time of the current
;; implementation is.

;; A (partially-sorted-tree/c E) is a partially sorted tree of elements of
;; type E. A partially sorted tree is like a binary tree, but instead of being
;; fully sorted with an element, a subtree of smaller elements, and a subtree of
;; greater elements, the greater elements are left unsorted. Elements smaller
;; than the pivot it are recursively partially sorted. This representation
;; allows efficient lazy traversal of the tree from least to greatest, because
;; the lesser subtree contains all elements smaller than the pivot and can be
;; iterated without sorting elements larger than the pivot.
(define-record-type partially-sorted-tree
  ;; E
  ;; The tree's pivot element, used to partition other elements
  (pivot-element

   ;; (or/c (partially-sorted-tree/c E) empty-tree? unsorted-stack?)
   ;; A subtree containing all elements that are smaller than this tree's pivot,
   ;; or the empty tree if there are no such elements. Additionally, the subtree
   ;; may be an unsorted stack. This occurs when the minimum values are trimmed
   ;; from the subtree until the subtree's greater-stack is all that remains. We
   ;; allow the greater-stack to be stored here so we can delay reversing and
   ;; partially sorting it until elements from it are actually needed.
   lesser-subtree

   ;; (listof E)
   ;; Reverse-ordered elements equivalent to pivot. Equivalent elements are
   ;; stored in reverse order to allow efficient insertion into the head of a
   ;; list. However, this requires reversing the list before iterating
   ;; equivalent elements in order to preserve encounter order.
   equivalent-stack

   ;; (listof E)
   ;; Reverse-ordered elements greater than pivot. Stored in reverse order for
   ;; the same reasons as for equivalent-stack.
   greater-stack))

(define-wrapper-type unsorted-stack)

(define-singleton-type empty-tree)

(define-record-type tree-trimming (minimum-leaves leftover-tree))

(define/guard (tree-trim-minimum possibly-unbuilt-tree comparator)
  (guard (not (partially-sorted-tree? possibly-unbuilt-tree)) #:else
    (partially-sorted-tree-trim-minimum possibly-unbuilt-tree comparator))
  (define elements
    (list-reverse (unsorted-stack-value possibly-unbuilt-tree)))
  (define tree
    (for/fold ([built-tree empty-tree]) ([element (in-list elements)])
      (tree-insert built-tree element #:comparator comparator)))
  (partially-sorted-tree-trim-minimum tree comparator))

(define/guard (partially-sorted-tree-trim-minimum tree comparator)
  (define pivot-element (partially-sorted-tree-pivot-element tree))
  (define lesser-subtree (partially-sorted-tree-lesser-subtree tree))
  (define equivalent-stack (partially-sorted-tree-equivalent-stack tree))
  (define greater-stack (partially-sorted-tree-greater-stack tree))
  (guard (not (empty-tree? lesser-subtree)) #:else
    (define leaves (list-insert (list-reverse equivalent-stack) pivot-element))
    (define leftovers
      (if (empty-list? greater-stack)
          empty-tree
          (unsorted-stack greater-stack)))
    (tree-trimming #:minimum-leaves leaves #:leftover-tree leftovers))
  (define subtree-trimming (tree-trim-minimum lesser-subtree comparator))
  (define leaves (tree-trimming-minimum-leaves subtree-trimming))
  (define leftovers
    (partially-sorted-tree
     #:pivot-element pivot-element
     #:lesser-subtree (tree-trimming-leftover-tree subtree-trimming)
     #:equivalent-stack equivalent-stack
     #:greater-stack greater-stack))
  (tree-trimming #:minimum-leaves leaves #:leftover-tree leftovers))

(define (singleton-tree element)
  (partially-sorted-tree
   #:pivot-element element
   #:lesser-subtree empty-tree
   #:equivalent-stack empty-list
   #:greater-stack empty-list))

(define/guard (tree-insert tree element #:comparator comparator)
  (guard (not (empty-tree? tree)) #:else
    (singleton-tree element))
  (define pivot (partially-sorted-tree-pivot-element tree))
  (define lesser-subtree (partially-sorted-tree-lesser-subtree tree))
  (define equivalent-stack (partially-sorted-tree-equivalent-stack tree))
  (define greater-stack (partially-sorted-tree-greater-stack tree))
  (define comparison-to-pivot (compare comparator element pivot))

  (guard (not (equal? comparison-to-pivot equivalent)) #:else
    (partially-sorted-tree
     #:pivot-element pivot
     #:lesser-subtree lesser-subtree
     #:equivalent-stack (list-insert equivalent-stack element)
     #:greater-stack greater-stack))
  
  (guard (not (equal? comparison-to-pivot greater)) #:else
    (partially-sorted-tree
     #:pivot-element pivot
     #:lesser-subtree lesser-subtree
     #:equivalent-stack equivalent-stack
     #:greater-stack (list-insert greater-stack element)))
  
  (define new-subtree (tree-insert lesser-subtree element #:comparator comparator))
  (partially-sorted-tree
   #:pivot-element pivot
   #:lesser-subtree new-subtree
   #:equivalent-stack equivalent-stack
   #:greater-stack greater-stack))

(define (sorting [comparator real<=>]
                 #:key [key-function values]
                 #:descending? [descending? #f])
  ;; TODO(https://github.com/jackfirth/rebellion/issues/301): consider handling
  ;;   key function more efficiently by caching keys.
  (define keyed-comparator
    (comparator-map (if descending? (comparator-reverse comparator) comparator)
                    key-function))

  (define (start) (variant #:consume empty-tree))

  (define (consume state element)
    (define next-state
      (cond
        [(empty-tree? state)
         (partially-sorted-tree
          #:pivot-element element
          #:lesser-subtree empty-tree
          #:equivalent-stack empty-list
          #:greater-stack empty-list)]
        [(partially-sorted-tree? state)
         (tree-insert state element #:comparator keyed-comparator)]
        [else
         (raise-arguments-error 'sorting "expected a tree"
                                "actual previous state" state)]))
    (variant #:consume next-state))
  
  (define/guard (half-close tree)
    (if (empty-tree? tree)
        (variant #:finish #false)
        (variant #:half-closed-emit tree)))
  
  (define (half-closed-emit state)
    (define trimming
      (if (tree-trimming? state)
          state
          (tree-trim-minimum state keyed-comparator)))
    (define minimum (list-first (tree-trimming-minimum-leaves trimming)))
    (define remaining-leaves
      (list-rest (tree-trimming-minimum-leaves trimming)))
    (define tree (tree-trimming-leftover-tree trimming))
    (define next-state
      (cond
        [(not (empty-list? remaining-leaves))
         (variant #:half-closed-emit
                  (tree-trimming #:minimum-leaves remaining-leaves
                                 #:leftover-tree tree))]
        [(empty-tree? tree) (variant #:finish #f)]
        [(or (partially-sorted-tree? tree) (unsorted-stack? tree))
         (variant #:half-closed-emit tree)]
        [else (raise-arguments-error 'sorting "expected a tree"
                                     "actual" tree)]))
    (half-closed-emission next-state minimum))
  
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter impossible
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name 'sorting))
