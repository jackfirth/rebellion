#lang racket/base


(module+ test
  (provide
   check-mutable-rb-tree-structurally-equal?
   check-mutable-rb-tree-invariants))


(module+ test
  (require guard
           rackunit
           rebellion/collection/private/mutable-red-black-tree-base))


;@----------------------------------------------------------------------------------------------------


(module+ test

  (define-check (check-mutable-rb-tree-structurally-equal? actual expected)

    (define/guard (loop actual-node expected-node)

      (when (and (proper-mutable-rb-node? actual-node) (nil-leaf? expected-node))
        (with-check-info (['actual actual-node])
          (fail-check "unexpected node present")))
      
      (when (and (nil-leaf? actual-node) (proper-mutable-rb-node? expected-node))
        (with-check-info (['expected expected-node])
          (fail-check "node missing")))

      (when (and (proper-mutable-rb-node? actual-node) (proper-mutable-rb-node? expected-node))
        (with-check-info (['actual actual-node] ['expected expected-node])
          (define actual-color (mutable-rb-node-color actual-node))
          (define actual-key (mutable-rb-node-key actual-node))
          (define actual-value (mutable-rb-node-value actual-node))
          (define expected-color (mutable-rb-node-color expected-node))
          (define expected-key (mutable-rb-node-key expected-node))
          (define expected-value (mutable-rb-node-value expected-node))
          (unless (equal? actual-color expected-color)
            (fail-check "expected a node with a different color"))
          (unless (equal? actual-key expected-key)
            (fail-check "expected a node with a different key"))
          (unless (equal? actual-value expected-value)
            (fail-check "expected a node with a different value"))
          (loop
           (mutable-rb-node-child actual-node left)
           (mutable-rb-node-child expected-node left))
          (loop
           (mutable-rb-node-child actual-node right)
           (mutable-rb-node-child expected-node right)))))

    (loop (mutable-rb-tree-root-node actual) (mutable-rb-tree-root-node expected)))

  
  ;; Checks the red-black tree invariants: that red nodes only have black children, and that every
  ;; path from the root to a leaf must traverse the same number of black nodes.
  (define-check (check-mutable-rb-tree-invariants tree)

    (define/guard (check-red-node-children-are-black node)
      (guard (not (nil-leaf? node)) #:else
        (void))
      (define left-child (mutable-rb-node-child node left))
      (define right-child (mutable-rb-node-child node right))
      (when (red-node? node)
        (when (red-node? left-child)
          (with-check-info (['red-node node] ['left-child left-child])
            (fail-check
             "red nodes must have black child nodes, but this red node's left child is red")))
        (when (red-node? right-child)
          (with-check-info (['red-node node] ['right-child right-child])
            (fail-check
             "red nodes must have black child nodes, but this red node's right child is red"))))
      (check-red-node-children-are-black left-child)
      (check-red-node-children-are-black right-child))

    (define/guard (check-path-black-node-counts node)
      (guard (not (nil-leaf? node)) #:else
        0)
      (define left-count (check-path-black-node-counts (mutable-rb-node-child node left)))
      (define right-count (check-path-black-node-counts (mutable-rb-node-child node right)))
      (unless (equal? left-count right-count)
        (with-check-info (['node node]
                          ['left-black-count left-count]
                          ['right-black-count right-count])
          (fail-check
           "all paths through a red black tree must traverse the same number of black nodes")))
      (if (black-node? node) (add1 left-count) left-count))

    (define/guard (check-node-sizes node)
      (guard (not (nil-leaf? node)) #:else
        (void))
      (define size (mutable-rb-node-size node))
      (define left-child (mutable-rb-node-child node left))
      (define right-child (mutable-rb-node-child node right))
      (define left-size (mutable-rb-node-size left-child))
      (define right-size (mutable-rb-node-size right-child))
      (unless (equal? (+ left-size right-size 1) size)
        (with-check-info (['node node]
                          ['size size]
                          ['left-size left-size]
                          ['right-size right-size])
          (fail-check "the size recorded in each node must be equal to left-size + right-size + 1")))
      (check-node-sizes left-child)
      (check-node-sizes right-child))

    (define root (mutable-rb-tree-root-node tree))
    (check-path-black-node-counts root)
    (check-red-node-children-are-black root)
    (check-node-sizes root)))
