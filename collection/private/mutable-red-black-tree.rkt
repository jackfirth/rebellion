#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-red-black-tree? predicate/c]
  [make-mutable-red-black-tree (-> comparator? mutable-red-black-tree?)]
  [mutable-red-black-tree-size (-> mutable-red-black-tree? natural?)]
  [mutable-red-black-tree-comparator (-> mutable-red-black-tree? comparator?)]
  [mutable-red-black-tree-contains? (-> mutable-red-black-tree? any/c boolean?)]
  [mutable-red-black-tree-insert! (-> mutable-red-black-tree? any/c void?)]
  [mutable-red-black-tree-remove! (-> mutable-red-black-tree? any/c void?)]
  [mutable-red-black-tree-clear! (-> mutable-red-black-tree? void?)]
  [mutable-red-black-tree-elements (-> mutable-red-black-tree? list?)]
  [in-mutable-red-black-tree (-> mutable-red-black-tree? (sequence/c any/c))]))


(require racket/block
         racket/match
         racket/math
         racket/sequence
         racket/stream
         rebellion/base/comparator
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------
;; Mutable red-black trees. The algorithms used in this module are based on the ones in the Wikipedia
;; page on red black trees, see <https://en.wikipedia.org/wiki/Red-black_tree>.


;; We use constants for the red/black color enum instead of define-enum-type to avoid unnecessary
;; dependencies on other parts of Rebellion, especially cyclic dependencies. We define constants
;; instead of using the symbols directly so that typos are compile-time errors.
(define red 'red)
(define black 'black)


;; Same for the left/right enum.
(define left 'left)
(define right 'right)


(define (direction-inverse direction)
  (match direction
    [(== left) right]
    [(== right) left]))


(struct mutable-red-black-node
  ([parent #:mutable] [left-child #:mutable] [right-child #:mutable] [color #:mutable] element)
  #:constructor-name constructor:mutable-red-black-node)


(define (make-mutable-red-black-node element)
  (constructor:mutable-red-black-node #false #false #false red element))


(define (mutable-red-black-node-child node direction)
  (match direction
    [(== left) (mutable-red-black-node-left-child node)]
    [(== right) (mutable-red-black-node-right-child node)]))


(define (set-mutable-red-black-node-child! node child direction)
  (match direction
    [(== left) (set-mutable-red-black-node-left-child! node child)]
    [(== right) (set-mutable-red-black-node-right-child! node child)]))


(define (mutable-red-black-node-parent-direction node)
  (define parents-left-child
    (mutable-red-black-node-left-child (mutable-red-black-node-parent node)))
  (if (equal? parents-left-child node) left right))


(struct mutable-red-black-tree (comparator [root-node #:mutable] [size #:mutable])
  #:constructor-name constructor:mutable-red-black-tree)


(define (make-mutable-red-black-tree comparator)
  (constructor:mutable-red-black-tree comparator #false 0))


;; Mutable-Red-Black-Node -> (Sequence Any)
(define (in-mutable-red-black-tree-node node)
  (define element (mutable-red-black-node-element node))
  (define left (mutable-red-black-node-left-child node))
  (define right (mutable-red-black-node-right-child node))
  (cond
    [(and left right)
     (sequence-append
      (in-mutable-red-black-tree-node left) (stream element) (in-mutable-red-black-tree-node right))]
    [left (sequence-append (in-mutable-red-black-tree-node left) (stream element))]
    [right (sequence-append (stream element) (in-mutable-red-black-tree-node right))]
    [else (stream element)]))


;; Mutable-Red-Black-Tree -> (Sequence Any)
(define (in-mutable-red-black-tree tree)
  (stream*
   (block
    (define root (mutable-red-black-tree-root-node tree))
    (if root (in-mutable-red-black-tree-node root) (stream)))))


;; Mutable-Red-Black-Tree -> List
(define (mutable-red-black-tree-elements tree)
  (sequence->list (in-mutable-red-black-tree tree)))


(define (mutable-red-black-tree-contains? tree element)
  (define element<=> (mutable-red-black-tree-comparator tree))
  (define node (mutable-red-black-tree-root-node tree))
  
  (define/guard (loop node)
    (guard node else
      #false)
    (match (compare element<=> (mutable-red-black-node-element node) element)
      [(== equivalent) #true]
      [(== lesser) (loop (mutable-red-black-node-right-child node))]
      [(== greater) (loop (mutable-red-black-node-left-child node))]))
  
  (loop node))


(define (mutable-red-black-tree-rotate! tree subtree-root direction)
  (define subtree-parent (mutable-red-black-node-parent subtree-root))
  (define opposite-direction (direction-inverse direction))
  (define new-subtree-root (mutable-red-black-node-child subtree-root opposite-direction))
  (unless new-subtree-root
    (raise-arguments-error
     (name mutable-red-black-tree-rotate!)
     "cannot rotate subtree, child to use as new root does not exist"))
  (define child (mutable-red-black-node-child new-subtree-root direction))
  (set-mutable-red-black-node-child! subtree-root child opposite-direction)
  (when child
    (set-mutable-red-black-node-parent! child subtree-root))
  (set-mutable-red-black-node-child! new-subtree-root subtree-root direction)
  (set-mutable-red-black-node-parent! subtree-root new-subtree-root)
  (set-mutable-red-black-node-parent! new-subtree-root subtree-parent)
  (if subtree-parent
      (if (equal? (mutable-red-black-node-right-child subtree-parent) subtree-root)
          (set-mutable-red-black-node-right-child! subtree-parent new-subtree-root)
          (set-mutable-red-black-node-left-child! subtree-parent new-subtree-root))
      (set-mutable-red-black-tree-root-node! tree new-subtree-root)))


;; Mutable-Red-Black-Tree Any -> Void
;; Inserts an element into a red-black tree. The element must not already be present in the tree.
(define/guard (mutable-red-black-tree-insert! tree element)
  (define element<=> (mutable-red-black-tree-comparator tree))
  (define parent (mutable-red-black-tree-insertion-parent tree element))
  (guard parent else
    (set-mutable-red-black-tree-root-node! tree (make-mutable-red-black-node element)))
  (define direction
    (match (compare element<=> (mutable-red-black-node-element parent) element)
      [(== equivalent)
       (raise-arguments-error
        (name mutable-red-black-tree-insert!)
        "cannot insert element, tree already contains an equivalent element")]
      [(== lesser) right]
      [(== greater) left]))
  (set-mutable-red-black-tree-size! tree (add1 (mutable-red-black-tree-size tree)))
  (define node (make-mutable-red-black-node element))
  (set-mutable-red-black-node-parent! node parent)
  (set-mutable-red-black-node-child! parent node direction)
  
  (define/guard (rebalancing-loop node parent)
    
    (guard (equal? (mutable-red-black-node-color parent) red) else
      ;; Insertion case 3: parent is black. No rebalancing necessary, black parent node with new red
      ;; child node is fine.
      (void))
    
    (define grandparent (mutable-red-black-node-parent parent))
    
    (guard grandparent else
      ;; Insertion case 6: parent is red and root.
      (set-mutable-red-black-node-color! parent black))
    
    (define grandparent-parent-direction
      (if (equal? (mutable-red-black-node-left-child grandparent) parent) left right))
    (define grandparent-uncle-direction (direction-inverse grandparent-parent-direction))
    (define uncle
      (mutable-red-black-node-child grandparent grandparent-uncle-direction))
    
    (guard (or (not uncle) (equal? (mutable-red-black-node-color uncle) black)) then
      ;; Insertion cases 4 and 5: parent is red and uncle is black.
      (cond
        [(equal? (mutable-red-black-node-parent-direction node) grandparent-parent-direction)
         ;; Insertion case 5: parent is red, uncle is black, and this node is an "outer child" of the
         ;; parent.
         (mutable-red-black-tree-rotate! tree grandparent grandparent-uncle-direction)
         (set-mutable-red-black-node-color! parent black)
         (set-mutable-red-black-node-color! grandparent red)]
        [else
         ;; Insertion case 4: parent is red, uncle is black, and this node is an "inner child" of the
         ;; parent.
         (mutable-red-black-tree-rotate! tree parent grandparent-parent-direction)
         (define new-parent (mutable-red-black-node-child grandparent grandparent-parent-direction))
         (mutable-red-black-tree-rotate! tree grandparent grandparent-uncle-direction)
         (set-mutable-red-black-node-color! parent black)
         (set-mutable-red-black-node-color! grandparent red)]))
    
    ;; Insertion cases 1 and 2: parent and uncle both red. We repaint them black and repaint the
    ;; grandparent red, then rebalance the grandparent.
    (set-mutable-red-black-node-color! parent black)
    (set-mutable-red-black-node-color! uncle black)
    (set-mutable-red-black-node-color! grandparent red)
    (define great-grandparent (mutable-red-black-node-parent grandparent))
    (guard great-grandparent else
      ;; Insertion case 2: same as case 1, except the grandparent is the root so after repainting it
      ;; the tree is already fully balanced.
      (void))
    (rebalancing-loop grandparent great-grandparent))
  
  (rebalancing-loop node parent))


;; Mutable-Red-Black-Tree Any -> (U Mutable-Red-Black-Node False)
(define (mutable-red-black-tree-insertion-parent tree element)
  (define element<=> (mutable-red-black-tree-comparator tree))
  (define node (mutable-red-black-tree-root-node tree))
  
  (define (loop node)
    (define next-direction
      (match (compare element<=> (mutable-red-black-node-element node) element)
        [(== equivalent)
         (raise-arguments-error
          (name mutable-red-black-tree-insertion-parent)
          "cannot insert element, tree already contains an equivalent element")]
        [(== lesser) right]
        [(== greater) left]))
    (define next-node (mutable-red-black-node-child node next-direction))
    (if next-node (loop next-node) node))
  
  (and node (loop node)))


(define (mutable-red-black-tree-remove! tree element)
  ;; TODO
  (void))


(define (mutable-red-black-tree-clear! tree)
  (set-mutable-red-black-tree-root-node! tree #false)
  (set-mutable-red-black-tree-size! tree 0))


(module+ test
  (test-case (name-string mutable-red-black-tree-insert!)
    
    (test-case "insert one element into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 5)))
    
    (test-case "insert two ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-insert! tree 10)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 5 10)))
    
    (test-case "insert two descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-insert! tree 2)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 2 5)))
    
    (test-case "insert many ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-insert! tree 5)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert many descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 1)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert ascending and descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-insert! tree 1)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3)))
    
    (test-case "insert many ascending and descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-insert! tree 2)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert repeatedly ascending then descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-insert! tree 7)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 6)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-insert! tree 4)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))
    
    (test-case "insert repeatedly descending then ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 7)
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-insert! tree 6)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-insert! tree 4)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))
    
    (test-case "insert many ascending elements then many descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-insert! tree 6)
      (mutable-red-black-tree-insert! tree 7)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 1)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5 6 7))))
  
  (test-case (name-string mutable-red-black-tree-clear!)
    
    (test-case "clear should do nothing to an empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-clear! tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements '()))
    
    (test-case "clear should remove all elements from a tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-clear! tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements '()))
    
    (test-case "clear should set size to zero"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-clear! tree)
      (check-equal? (mutable-red-black-tree-size tree) 0))))
