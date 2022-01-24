#lang racket/base


(module+ test
  (require racket/sequence
           rackunit
           rebellion/base/comparator
           rebellion/collection/private/mutable-red-black-tree-base
           rebellion/collection/private/mutable-red-black-tree-insertion
           rebellion/collection/private/mutable-red-black-tree-iteration
           (submod rebellion/collection/private/testing/mutable-red-black-tree-invariants test)
           rebellion/private/static-name))


(module+ test
  (test-case (name-string mutable-rb-tree-clear!)
      
    (test-case "clear should do nothing to an empty tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-clear! tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries '()))
      
    (test-case "clear should remove all elements from a tree"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 1 'a)
      (mutable-rb-tree-put! tree 2 'b)
      (mutable-rb-tree-put! tree 3 'c)
      (mutable-rb-tree-clear! tree)
      (check-mutable-rb-tree-invariants tree)
      (define entries (sequence->list (in-mutable-rb-tree tree)))
      (check-equal? entries '()))
      
    (test-case "clear should set size to zero"
      (define tree (make-mutable-rb-tree natural<=>))
      (mutable-rb-tree-put! tree 1 'a)
      (mutable-rb-tree-put! tree 2 'b)
      (mutable-rb-tree-put! tree 3 'c)
      (mutable-rb-tree-clear! tree)
      (check-mutable-rb-tree-invariants tree)
      (check-equal? (mutable-rb-tree-size tree) 0))))
