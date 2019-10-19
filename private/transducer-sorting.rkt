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

   ;; (or/c (partially-sorted-tree/c E) empty-tree?)
   ;; A subtree containing all elements that are smaller than this tree's pivot,
   ;; or the empty tree if there are no such elements.
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

(define-singleton-type empty-tree)

(define-record-type tree-trimming (minimum-leaf leftover-tree))

(define (partially-sorted-tree-trim-minimum tree comparator key-function)
  #f)

(define (sorting [comparator real<=>] #:key [key-function values])
  ;; TODO(https://github.com/jackfirth/): handle key function more efficiently
  ;;   by caching keys, and avoid computing them altogether when possible (e.g.
  ;;   singleton sequence).
  (define keyed-comparator (comparator-map real<=> key-function))
  
  (define (start)
    (variant #:consume empty-tree))
  (define (consume state element)
    (define next-state
      (cond
        [(empty-tree? state)
         (partially-sorted-tree
          #:pivot-element element
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
      (partially-sorted-tree-trim-minimum tree keyed-comparator))
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
