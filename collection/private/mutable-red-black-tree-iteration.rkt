#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [in-mutable-rb-tree (->* (mutable-rb-tree?) (#:descending? boolean?) (sequence/c entry?))]
  [in-mutable-rb-tree-keys (->* (mutable-rb-tree?) (#:descending? boolean?) (sequence/c any/c))]
  [in-mutable-rb-tree-values (->* (mutable-rb-tree?) (#:descending? boolean?) (sequence/c any/c))]
  [in-mutable-rb-subtree (->* (mutable-rb-tree? range?) (#:descending? boolean?) (sequence/c entry?))]
  [in-mutable-rb-subtree-keys
   (->* (mutable-rb-tree? range?) (#:descending? boolean?) (sequence/c any/c))]
  [in-mutable-rb-subtree-values
   (->* (mutable-rb-tree? range?) (#:descending? boolean?) (sequence/c any/c))]))


(require racket/block
         racket/sequence
         racket/stream
         rebellion/base/comparator
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/entry
         rebellion/collection/private/mutable-red-black-tree-base
         rebellion/private/guarded-block)


;@----------------------------------------------------------------------------------------------------


(define/guard (in-mutable-rb-tree-node node #:descending? [descending? #false])

  (define (recur node)
    (in-mutable-rb-tree-node node #:descending? descending?))

  (guard (nil-leaf? node) then
    (stream))
  (define entry (mutable-rb-node-entry node))
  (define true-left (mutable-rb-node-child node left))
  (define true-right (mutable-rb-node-child node right))
  (define left-child (if descending? true-right true-left))
  (define right-child (if descending? true-left true-right))
  (cond
    [(and (proper-mutable-rb-node? left-child) (proper-mutable-rb-node? right-child))
     (sequence-append (recur left-child) (stream entry) (recur right-child))]
    [(proper-mutable-rb-node? left-child) (sequence-append (recur left-child) (stream entry))]
    [(proper-mutable-rb-node? right-child) (sequence-append (stream entry) (recur right-child))]
    [else (stream entry)]))


(define (in-mutable-rb-tree tree #:descending? [descending? #false])
  (stream* (in-mutable-rb-tree-node (mutable-rb-tree-root-node tree) #:descending? descending?)))


(define (in-mutable-rb-tree-keys tree #:descending? [descending? #false])
  (for/stream ([e (in-mutable-rb-tree tree #:descending? descending?)])
    (entry-key e)))


(define (in-mutable-rb-tree-values tree #:descending? [descending? #false])
  (for/stream ([e (in-mutable-rb-tree tree #:descending? descending?)])
    (entry-value e)))


(define/guard (in-mutable-rb-subtree-node node key-range #:descending? [descending? #false])

  (define (recur node)
    (in-mutable-rb-subtree-node node key-range #:descending? descending?))

  (guard (nil-leaf? node) then
    (stream))
  
  (define key (mutable-rb-node-key node))
  (define range-comparison (range-compare-to-value key-range key))
  (define true-left
    (and (not (equal? range-comparison greater)) (mutable-rb-node-child node left)))
  (define true-right
    (and (not (equal? range-comparison lesser)) (mutable-rb-node-child node right)))
  (define left-child (if descending? true-right true-left))
  (define right-child (if descending? true-left true-right))
  (define left-stream
    (if (proper-mutable-rb-node? left-child) (stream* (recur left-child)) (stream)))
  (define right-stream
    (if (proper-mutable-rb-node? right-child) (stream* (recur right-child)) (stream)))
  (define entry-stream
    (if (equal? range-comparison equivalent)
        (stream (entry key (mutable-rb-node-value node)))
        (stream)))
  (sequence-append left-stream entry-stream right-stream))


(define (in-mutable-rb-subtree tree key-range #:descending? [descending? #false])
  (stream*
   (in-mutable-rb-subtree-node (mutable-rb-tree-root-node tree) key-range #:descending? descending?)))


(define (in-mutable-rb-subtree-keys tree key-range #:descending? [descending? #false])
  (for/stream ([e (in-mutable-rb-subtree tree key-range #:descending? descending?)])
    (entry-key e)))


(define (in-mutable-rb-subtree-values tree key-range #:descending? [descending? #false])
  (for/stream ([e (in-mutable-rb-subtree tree key-range #:descending? descending?)])
    (entry-value e)))
