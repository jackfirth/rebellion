#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-rb-tree-remove! (-> mutable-rb-tree? any/c void?)]))


(require rebellion/collection/private/mutable-red-black-tree-base
         rebellion/collection/private/mutable-red-black-tree-search
         guard
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rebellion/base/comparator
           rebellion/collection/private/testing/literal-mutable-red-black-tree
           (submod rebellion/collection/private/testing/mutable-red-black-tree-invariants test)
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define/guard (mutable-rb-tree-remove! tree key)
  (define node (mutable-rb-tree-get-node tree key))
  (unless (nil-leaf? node)
    (mutable-rb-tree-remove-node! tree node)))


(define/guard (mutable-rb-tree-remove-node! tree node)
  (define root? (root-node? node))
  (define left-child (mutable-rb-node-child node left))
  (define right-child (mutable-rb-node-child node right))

  ;; First we check for the simple cases that don't require any rebalancing.
  (guard (not (and root? (nil-leaf? left-child) (nil-leaf? right-child))) #:else
    (mutable-rb-tree-clear! tree))

  (guard (not (and (proper-mutable-rb-node? left-child) (proper-mutable-rb-node? right-child))) #:else
    (define choose-left? (>= (mutable-rb-node-size left-child) (mutable-rb-node-size right-child)))
    (define child
      (if choose-left?
          (mutable-rb-node-max-child left-child)
          (mutable-rb-node-min-child right-child)))
    (mutable-rb-node-swap-contents! node child)
    (mutable-rb-tree-remove-node! tree child))

  (define color (mutable-rb-node-color node))

  (guard (not (equal? color red)) #:else
    (if root?
        (mutable-rb-tree-clear! tree)
        (mutable-rb-node-remove-from-parent! node)))

  (guard (not (proper-mutable-rb-node? left-child)) #:else
    (mutable-rb-node-swap-contents! node left-child)
    (mutable-rb-tree-remove-node! tree left-child))

  (guard (not (proper-mutable-rb-node? right-child)) #:else
    (mutable-rb-node-swap-contents! node right-child)
    (mutable-rb-tree-remove-node! tree right-child))

  ;; This is the complex case: the node to remove is a black non-root leaf. Removal will leave the
  ;; tree unbalanced.
  (define parent (mutable-rb-node-parent node))
  (define dir (mutable-rb-node-parent-direction node))
  
  (define (rebalance! node)
    (cond

      [(deletion-case1? node)
       (deletion-case1! node)
       (rebalance! (mutable-rb-node-parent node))]
      
      [(deletion-case2? node)
       (void)]
      
      [(deletion-case3? node)
       (deletion-case3! tree node)
       (rebalance! node)]
      
      [(deletion-case4? node)
       (deletion-case4! node)]
      
      [(deletion-case5? node)
       (deletion-case5! tree node)
       (rebalance! node)]
      
      [(deletion-case6? node)
       (deletion-case6! tree node)
       (rebalance! node)]))

  (rebalance! node)
  (mutable-rb-node-remove-from-parent! node))


(define/guard (deletion-case1? node)
  (and (black-node? (mutable-rb-node-parent node))
       (black-node? (mutable-rb-node-sibling node))
       (black-node? (mutable-rb-node-close-nephew node))
       (black-node? (mutable-rb-node-distant-nephew node))))


(define (deletion-case1! node)
  (mutable-rb-node-repaint! (mutable-rb-node-sibling node) red))


(define (deletion-case2? node)
  (mutable-rb-root? (mutable-rb-node-parent node)))


(define (deletion-case3? node)
  (and (black-node? (mutable-rb-node-parent node))
       (red-node? (mutable-rb-node-sibling node))
       (black-node? (mutable-rb-node-close-nephew node))
       (black-node? (mutable-rb-node-distant-nephew node))))


(define (deletion-case3! tree node)
  (define S (mutable-rb-node-sibling node))
  (define P (mutable-rb-node-parent node))
  (mutable-rb-node-repaint! S black)
  (mutable-rb-node-repaint! P red)
  (mutable-rb-node-rotate-relative! P S))


(define (deletion-case4? node)
  (and (red-node? (mutable-rb-node-parent node))
       (black-node? (mutable-rb-node-sibling node))
       (black-node? (mutable-rb-node-close-nephew node))
       (black-node? (mutable-rb-node-distant-nephew node))))


(define (deletion-case4! node)
  (mutable-rb-node-repaint! (mutable-rb-node-sibling node) red)
  (mutable-rb-node-repaint! (mutable-rb-node-parent node) black))


(define (deletion-case5? node)
  (and (black-node? (mutable-rb-node-sibling node))
       (red-node? (mutable-rb-node-close-nephew node))
       (black-node? (mutable-rb-node-distant-nephew node))))


(define (deletion-case5! tree node)
  (define S (mutable-rb-node-sibling node))
  (define C (mutable-rb-node-close-nephew node))
  (mutable-rb-node-repaint! S red)
  (mutable-rb-node-repaint! C black)
  (mutable-rb-node-rotate-relative! S C))


(define (deletion-case6? node)
  (and (black-node? (mutable-rb-node-sibling node))
       (red-node? (mutable-rb-node-distant-nephew node))))


(define (deletion-case6! tree node)
  (define P (mutable-rb-node-parent node))
  (define S (mutable-rb-node-sibling node))
  (define D (mutable-rb-node-distant-nephew node))
  (mutable-rb-node-repaint! S (mutable-rb-node-color P))
  (mutable-rb-node-repaint! P black)
  (mutable-rb-node-repaint! D black)
  (mutable-rb-node-rotate-relative! P S))


(define/guard (mutable-rb-node-sibling node)
  (define parent (mutable-rb-node-parent node))
  (guard (not (mutable-rb-root? parent)) #:else
    #false)
  (define parents-left-child (mutable-rb-node-child parent left))
  (if (equal? parents-left-child node)
      (and (proper-mutable-rb-node? parent) (mutable-rb-node-child parent right))
      parents-left-child))


(define/guard (mutable-rb-node-close-nephew node)
  (define parent (mutable-rb-node-parent node))
  (guard (not (mutable-rb-root? parent)) #:else
    #false)
  (define parents-left-child (mutable-rb-node-child parent left))
  (cond
    [(equal? parents-left-child node)
     (define sibling (mutable-rb-node-child parent right))
     (and (proper-mutable-rb-node? sibling) (mutable-rb-node-child sibling left))]
    [else
     (and (proper-mutable-rb-node? parents-left-child)
          (mutable-rb-node-child parents-left-child right))]))


(define/guard (mutable-rb-node-distant-nephew node)
  (define parent (mutable-rb-node-parent node))
  (guard (not (mutable-rb-root? parent)) #:else
    #false)
  (define parents-left-child (mutable-rb-node-child parent left))
  (cond
    [(equal? parents-left-child node)
     (define sibling (mutable-rb-node-child parent right))
     (and (proper-mutable-rb-node? sibling) (mutable-rb-node-child sibling right))]
    [else
     (and (proper-mutable-rb-node? parents-left-child)
          (mutable-rb-node-child parents-left-child left))]))


(module+ test
  (test-case "mutable-red-black-tree-remove!"

    (test-case "rebalancing deletion"

      (define N 1)
      (define P 2)
      (define C 3)
      (define S 4)
      (define D 5)

      (test-case "deletion case 1"
        (define tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black P
                  (black N)
                  (black S (black C) (black D)))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case1? node))
        (deletion-case1! node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black P
                  (black N)
                  (red S (black C) (black D)))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree))

      (test-case "deletion case 2"
        (define tree (mutable-rbtree! #:key-comparator natural<=> (black N)))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case2? node)))

      (test-case "deletion case 3"
        (define tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black P
                  (black N)
                  (red S (black C) (black D)))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case3? node))
        (deletion-case3! tree node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black S
                  (red P (black N) (black C))
                  (black D))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree))

      (test-case "deletion case 4"
        (define tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (red P
                (black N)
                (black S (black C) (black D)))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case4? node))
        (deletion-case4! node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black P
                  (black N)
                  (red S (black C) (black D)))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree))

      (test-case "deletion case 5"
        (for ([parent-color (in-list (list black red))])
          (with-check-info (['parent-color parent-color])
            (define tree
              (mutable-rbtree!
               #:key-comparator natural<=>
               (parent-color P
                             (black N)
                             (black S (red C) (black D)))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case5? node))
        (deletion-case5! tree node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (parent-color P
                         (black N)
                         (black C NIL (red S NIL (black D))))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree))))

      (test-case "deletion case 6"
        (for* ([parent-color (in-list (list black red))]
               [close-nephew-color (in-list (list black red))])
          (with-check-info (['parent-color parent-color] ['close-nephew-color close-nephew-color])
            (define tree
              (mutable-rbtree!
               #:key-comparator natural<=>
               (parent-color P
                             (black N)
                             (black S (close-nephew-color C) (red D)))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case6? node))
        (deletion-case6! tree node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (parent-color S
                         (black P (black N) (close-nephew-color C))
                         (black D))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree)))))

    (test-case "rebalancing deletion mirror images"

      (define N 5)
      (define P 4)
      (define C 3)
      (define S 2)
      (define D 1)

      (test-case "deletion case 1 mirror image"
        (define tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black P
                  (black S (black D) (black C))
                  (black N))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case1? node))
        (deletion-case1! node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black P
                  (red S (black D) (black C))
                  (black N))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree))

      (test-case "deletion case 3 mirror image"
        (define tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black P
                  (red S (black D) (black C))
                  (black N))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case3? node))
        (deletion-case3! tree node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black S
                  (black D)
                  (red P (black C) (black N)))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree))

      (test-case "deletion case 4 mirror image"
        (define tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (red P
                (black S (black D) (black C))
                (black N))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case4? node))
        (deletion-case4! node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (black P
                  (red S (black D) (black C))
                  (black N))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree))

      (test-case "deletion case 5 mirror image"
        (for ([parent-color (in-list (list black red))])
          (with-check-info (['parent-color parent-color])
            (define tree
              (mutable-rbtree!
               #:key-comparator natural<=>
               (parent-color P
                             (black S (black D) (red C))
                             (black N))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case5? node))
        (deletion-case5! tree node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (parent-color P
                         (black C (red S (black D) NIL) NIL)
                         (black N))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree))))

      (test-case "deletion case 6 mirror image"
        (for* ([parent-color (in-list (list black red))]
               [close-nephew-color (in-list (list black red))])
          (with-check-info (['parent-color parent-color] ['close-nephew-color close-nephew-color])
            (define tree
              (mutable-rbtree!
               #:key-comparator natural<=>
               (parent-color P
                             (black S (red D) (close-nephew-color C))
                             (black N))))
        (define node (mutable-rb-tree-get-node tree N))

        (check-true (deletion-case6? node))
        (deletion-case6! tree node)

        (define expected-tree
          (mutable-rbtree!
           #:key-comparator natural<=>
           (parent-color S
                         (black D)
                         (black P (close-nephew-color C) (black N)))))
        (check-mutable-rb-tree-structurally-equal? tree expected-tree)))))))
