#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [sorting (->* () (comparator? #:key (-> any/c any/c)) transducer?)]))

(require racket/bool
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/variant
         rebellion/collection/list
         rebellion/private/impossible
         rebellion/streaming/transducer/base
         rebellion/type/record
         rebellion/type/singleton)

;@------------------------------------------------------------------------------

;; The implementation of sorting transducers is lazily efficient; if only the
;; first k elements of a sorted sequence of n elements are consumed then the
;; amount of work done is O(n log k), rather than the O(n log n) cost of a full
;; sort. This property is achieved by using partially sorted trees. A partially
;; sorted tree is like a binary sort tree, but instead of being fully sorted
;; with an element, a subtree of smaller elements, and a subtree of greater
;; elements, elements larger than the pivot are left unsorted. Elements smaller
;; than the pivot it are recursively partially sorted. This representation
;; allows efficient lazy traversal of the tree from least to greatest, because
;; the lesser subtree contains all elements smaller than the pivot and can be
;; iterated without sorting elements larger than the pivot.

;; A (partially-sorted-tree/c K E) is a partially sorted tree of elements of
;; type E that are compared by their keys of type K.
(define-record-type partially-sorted-tree
  ;; E
  ;; The tree's pivot element, used to partition other elements
  (pivot-element

   ;; (option/c K)
   ;; The pivot's key, as computed by a key function. Starts absent to allow
   ;; lazy key construction.
   pivot-key

   ;; (partially-sorted-tree/c K E)
   ;; A subtree containing all elements that are smaller than this tree's pivot.
   ;; Every element in this subtree has its key computed.
   lesser-subtree

   ;; (listof E)
   ;; Reverse-ordered elements equivalent to pivot. Equivalent elements are
   ;; stored in reverse order to allow efficient insertion into the head of a
   ;; list. However, this requires reversing the list before iterating
   ;; equivalent elements in order to preserve encounter order. Note that keys
   ;; for these elements were computed, but then discarded because there's no
   ;; need to compare them to anything anymore.
   equivalent-stack

   ;; (listof (entry/c K E))
   ;; Reverse-ordered elements greater than pivot. Stored in reverse order for
   ;; the same reasons as for equivalent-stack. Keys are stored alongside
   ;; elements because further sorting is required and keys should only be
   ;; computed once.
   greater-stack))

;; TODO: finish implementing this so I can confirm that this approach actually
;; works.

(define-singleton-type empty-tree)

(define-record-type tree-trimming (minimum-leaf leftover-tree))

(define (partially-sorted-tree-trim-minimum tree comparator key-function)
  #f)

(define (sorting [comparator real<=>] #:key [key-function values])
  (define (start)
    (variant #:consume empty-tree))
  (define (consume state element)
    (define next-state
      (cond
        [(empty-tree? state)
         (partially-sorted-tree
          #:pivot-element element
          #:pivot-key absent
          #:lesser-subtree empty-tree
          #:equivalent-stack empty-list
          #:greater-stack empty-list)]
        [else #f]))
    (variant #:consume next-state))
  (define (half-close tree)
    (cond
      [(empty-tree? tree) (variant #:finish #f)]
      [else (variant #:half-closed-emit tree)]))
  (define (half-closed-emit tree)
    (define trimming
      (partially-sorted-tree-trim-minimum tree comparator key-function))
    (define minimum (tree-trimming-minimum-leaf trimming))
    (define next-tree (tree-trimming-leftover-tree trimming))
    (define next-state
      (cond
        [(empty-tree? next-tree) (variant #:finish #f)]
        [else (variant #:half-closed-emit next-tree)]))
    (emission minimum next-state))
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter impossible
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name 'sorting))
