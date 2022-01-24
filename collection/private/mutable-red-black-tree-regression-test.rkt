#lang racket/base


(module+ test
  (require racket/sequence
           rackunit
           rebellion/base/comparator
           rebellion/collection/private/mutable-red-black-tree-base
           rebellion/collection/private/mutable-red-black-tree-insertion
           rebellion/collection/private/mutable-red-black-tree-iteration
           rebellion/collection/private/mutable-red-black-tree-deletion
           (submod rebellion/collection/private/testing/mutable-red-black-tree-invariants test)))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case "regression test #509"
    ;; See https://github.com/jackfirth/rebellion/issues/509
    (define tree (make-mutable-rb-tree real<=>))
    (mutable-rb-tree-put! tree 4 #false)
    (mutable-rb-tree-put! tree 6 #false)
    (mutable-rb-tree-put! tree 7 #false)
    (mutable-rb-tree-put! tree 5 #false)
    (mutable-rb-tree-put! tree 1 #false)
    (mutable-rb-tree-put! tree 2 #false)
    (mutable-rb-tree-put! tree 3 #false)
    (mutable-rb-tree-remove! tree 6)
    (mutable-rb-tree-remove! tree 7)
    (check-mutable-rb-tree-invariants tree)
    (check-equal? (sequence->list (in-mutable-rb-tree-keys tree)) (list 1 2 3 4 5))))
