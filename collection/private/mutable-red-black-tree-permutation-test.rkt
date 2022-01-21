#lang racket/base


(module+ test
  (require racket/list
           racket/sequence
           rackunit
           rebellion/base/comparator
           rebellion/collection/private/mutable-red-black-tree-base
           rebellion/collection/private/mutable-red-black-tree-deletion
           rebellion/collection/private/mutable-red-black-tree-insertion
           rebellion/collection/private/mutable-red-black-tree-iteration
           (submod rebellion/collection/private/testing/mutable-red-black-tree-invariants test)))


;@----------------------------------------------------------------------------------------------------


(module+ test

  (test-case "permutation test"
    (for* ([max-size (in-range 1 6)]
           [keys (in-value (range 1 (add1 max-size)))]
           [insertion-order (in-permutations keys)]
           [deletion-order (in-permutations keys)])
      (define tree (make-mutable-rb-tree natural<=>))
      (with-check-info (['tree tree])

        (for/fold ([inserted-keys '()]
                   #:result (void))
                  ([key (in-list insertion-order)]
                   [inserted-key-count (in-naturals 1)])
          (mutable-rb-tree-put! tree key #false)
          (let ([inserted-keys (append inserted-keys (list key))])
            (with-check-info (['inserted-keys inserted-keys])

              (check-mutable-rb-tree-invariants tree)

              (check-equal?
               (sequence->list (in-mutable-rb-tree-keys tree))
               (sort inserted-keys <)
               "inserted keys should appear in sorted order when the tree is iterated")

              (check-equal? (mutable-rb-tree-size tree) inserted-key-count
                            "inserting a key should increase the tree's size")
            
              inserted-keys)))

        (with-check-info (['inserted-keys insertion-order])
          (for/fold ([remaining-keys keys]
                     [deleted-keys '()]
                     #:result (void))
                    ([key (in-list deletion-order)]
                     [deleted-key-count (in-naturals 1)])
            (with-check-info (['deleted-keys deleted-keys]
                              ['next-deleted-key key])
              (mutable-rb-tree-remove! tree key))
            (let ([remaining-keys (remove key remaining-keys)]
                  [deleted-keys (append deleted-keys (list key))])
              (with-check-info (['deleted-keys deleted-keys])

                (check-mutable-rb-tree-invariants tree)

                (check-equal? (sequence->list (in-mutable-rb-tree-keys tree)) remaining-keys
                              "removed keys should no longer appear when the tree is iterated")

                (check-equal? (mutable-rb-tree-size tree) (- max-size deleted-key-count)
                              "removing a key should decrease the tree's size")

                (values remaining-keys deleted-keys)))))))))
