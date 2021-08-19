#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-red-black-tree? predicate/c]
  [make-mutable-red-black-tree (-> comparator? mutable-red-black-tree?)]
  [mutable-red-black-tree-size (-> mutable-red-black-tree? natural?)]
  [mutable-red-black-tree-comparator (-> mutable-red-black-tree? comparator?)]
  [mutable-red-black-tree-contains? (-> mutable-red-black-tree? any/c boolean?)]
  [mutable-red-black-tree-least-element (-> mutable-red-black-tree? option?)]
  [mutable-red-black-tree-greatest-element (-> mutable-red-black-tree? option?)]
  [mutable-red-black-tree-element-less-than (-> mutable-red-black-tree? any/c option?)]
  [mutable-red-black-tree-element-greater-than (-> mutable-red-black-tree? any/c option?)]
  [mutable-red-black-tree-element-at-most (-> mutable-red-black-tree? any/c option?)]
  [mutable-red-black-tree-element-at-least (-> mutable-red-black-tree? any/c option?)]
  [mutable-red-black-tree-add! (-> mutable-red-black-tree? any/c void?)]
  [mutable-red-black-tree-remove! (-> mutable-red-black-tree? any/c void?)]
  [mutable-red-black-tree-clear! (-> mutable-red-black-tree? void?)]
  [mutable-red-black-tree-elements (-> mutable-red-black-tree? list?)]
  [mutable-red-black-subtree-size (-> mutable-red-black-tree? range? natural?)]
  [mutable-red-black-subtree-clear! (-> mutable-red-black-tree? range? void?)]
  [in-mutable-red-black-tree
   (->* (mutable-red-black-tree?) (#:descending? boolean?) (sequence/c any/c))]
  [in-mutable-red-black-subtree
   (->* (mutable-red-black-tree? range?) (#:descending? boolean?) (sequence/c any/c))]))


(require racket/block
         racket/match
         racket/math
         racket/sequence
         racket/stream
         racket/struct
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/private/vector-binary-search
         rebellion/private/cut
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           racket/list
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
  ([parent #:mutable]
   [left-child #:mutable]
   [right-child #:mutable]
   [color #:mutable]
   [element #:mutable]
   ;; TODO: track size at each node
   [size #:mutable])
  
  #:constructor-name constructor:mutable-red-black-node

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) (name mutable-red-black-node))
      (λ (this)
        (list
         (mutable-red-black-node-color this)
         (mutable-red-black-node-element this)
         (mutable-red-black-node-left-child this)
         (mutable-red-black-node-right-child this)))))]

  #:property prop:custom-print-quotable 'never)


(define (make-mutable-red-black-node element)
  (constructor:mutable-red-black-node #false #false #false red element 1))


(define (mutable-red-black-node-child node direction)
  (match direction
    [(== left) (mutable-red-black-node-left-child node)]
    [(== right) (mutable-red-black-node-right-child node)]))


(define (set-mutable-red-black-node-child! node child direction)
  (match direction
    [(== left) (set-mutable-red-black-node-left-child! node child)]
    [(== right) (set-mutable-red-black-node-right-child! node child)]))


;; Determines the direction from the node's parent to the node.
(define (mutable-red-black-node-parent-direction node)
  (define parents-left-child
    (mutable-red-black-node-left-child (mutable-red-black-node-parent node)))
  (if (equal? parents-left-child node) left right))


;; Removes the pointer from the node's parent to the node.
(define (mutable-red-black-node-unlink-parent! node)
  (define direction (mutable-red-black-node-parent-direction node))
  (set-mutable-red-black-node-child! (mutable-red-black-node-parent node) #false direction))


(struct mutable-red-black-tree (comparator [root-node #:mutable] [size #:mutable])
  #:constructor-name constructor:mutable-red-black-tree)


(define (make-mutable-red-black-tree comparator)
  (constructor:mutable-red-black-tree comparator #false 0))


;; Mutable-Red-Black-Node -> (Sequence Any)
(define (in-mutable-red-black-tree-node node #:descending? [descending? #false])

  (define (recur node)
    (in-mutable-red-black-tree-node node #:descending? descending?))
  
  (define element (mutable-red-black-node-element node))
  (define true-left (mutable-red-black-node-left-child node))
  (define true-right (mutable-red-black-node-right-child node))
  (define left (if descending? true-right true-left))
  (define right (if descending? true-left true-right))
  (cond
    [(and left right) (sequence-append (recur left) (stream element) (recur right))]
    [left (sequence-append (recur left) (stream element))]
    [right (sequence-append (stream element) (recur right))]
    [else (stream element)]))


;; Mutable-Red-Black-Tree -> (Sequence Any)
(define (in-mutable-red-black-tree tree #:descending? [descending? #false])
  (stream*
   (block
    (define root (mutable-red-black-tree-root-node tree))
    (if root (in-mutable-red-black-tree-node root #:descending? descending?) (stream)))))


;; Mutable-Red-Black-Node -> (Sequence Any)
(define/guard (in-mutable-red-black-subtree-node
               node element-range #:descending? [descending? #false])

  (define (recur node)
    (in-mutable-red-black-subtree-node node element-range #:descending? descending?))
  
  (define element (mutable-red-black-node-element node))
  (guard (range-contains? element-range element) else
    (stream))
  (define true-left (mutable-red-black-node-left-child node))
  (define true-right (mutable-red-black-node-right-child node))
  (define left (if descending? true-right true-left))
  (define right (if descending? true-left true-right))
  (cond
    [(and left right) (sequence-append (recur left) (stream element) (recur right))]
    [left (sequence-append (recur left) (stream element))]
    [right (sequence-append (stream element) (recur right))]
    [else (stream element)]))


;; Mutable-Red-Black-Tree -> (Sequence Any)
(define (in-mutable-red-black-subtree tree element-range #:descending? [descending? #false])
  (stream*
   (block
    (define root (mutable-red-black-tree-root-node tree))
    (if root
        (in-mutable-red-black-subtree-node root element-range #:descending? descending?)
        (stream)))))


;; Mutable-Red-Black-Tree -> List
(define (mutable-red-black-tree-elements tree)
  (sequence->list (in-mutable-red-black-tree tree)))


(define (mutable-red-black-tree-contains? tree element)
  (and (mutable-red-black-tree-get-node tree element) #true))


(define (mutable-red-black-subtree-size tree element-range)
  ;; TODO
  0)


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


(define (mutable-red-black-tree-add! tree element)
  (unless (mutable-red-black-tree-contains? tree element)
    (mutable-red-black-tree-insert! tree element)))


;; Mutable-Red-Black-Tree Any -> Void
;; Inserts an element into a red-black tree. The element must not already be present in the tree.
(define/guard (mutable-red-black-tree-insert! tree element)
  (set-mutable-red-black-tree-size! tree (add1 (mutable-red-black-tree-size tree)))
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
         (set-mutable-red-black-node-color! new-parent black)
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


(define/guard (mutable-red-black-tree-remove! tree element)
  (define node (mutable-red-black-tree-get-node tree element))
  (guard node else
    (void))
  (set-mutable-red-black-tree-size! tree (sub1 (mutable-red-black-tree-size tree)))
  (mutable-red-black-tree-remove-node! tree node))


(define/guard (mutable-red-black-tree-remove-node! tree node)
  (define root? (not (mutable-red-black-node-parent node)))
  (define left (mutable-red-black-node-left-child node))
  (define right (mutable-red-black-node-right-child node))
  ;; First we check for the simple cases that don't require any rebalancing.
  (guard (and root? (not left) (not right)) then
    (mutable-red-black-tree-clear! tree))
  (guard (and left right) then
    (define choose-left? (even? (mutable-red-black-tree-size tree)))
    (define child
      (if choose-left?
          (mutable-red-black-node-max-child left)
          (mutable-red-black-node-min-child right)))
    (mutable-red-black-node-swap-contents! node child)
    (mutable-red-black-tree-remove-node! tree child))
  (define color (mutable-red-black-node-color node))
  (guard (equal? color red) then
    (when (or left right)
      (raise-arguments-error
       (name mutable-red-black-tree-remove-node!)
       "violation of red-black tree invariants"
       "node" node
       "color" color
       "root?" root?
       "left" left
       "left color" (and left (mutable-red-black-node-color left))
       "right" right
       "right color" (and right (mutable-red-black-node-color right))))
    (if root?
        (block
         (displayln (mutable-red-black-tree-elements tree))
         (mutable-red-black-tree-clear! tree))
        (mutable-red-black-node-unlink-parent! node)))
  (define only-child (or left right))
  (guard only-child then
    (mutable-red-black-node-swap-contents! node only-child)
    (mutable-red-black-tree-remove-node! tree only-child))

  ;; This is the complex case: the node to remove is a black non-root leaf. Removal will leave the
  ;; tree unbalanced.
  (define parent (mutable-red-black-node-parent node))
  (define dir (mutable-red-black-node-parent-direction node))
  (mutable-red-black-node-unlink-parent! node)

  (define/guard (rebalancing-loop node parent dir)
    (define inverse-dir (direction-inverse dir))
    (define sibling (mutable-red-black-node-child parent inverse-dir))

    (guard (equal? (mutable-red-black-node-color sibling) red) then
      ;; Deletion case 3: sibling is red, parent, close nephew, and distant nephew are all black.
      (mutable-red-black-tree-rotate! tree parent dir)
      (set-mutable-red-black-node-color! parent red)
      (set-mutable-red-black-node-color! sibling black)
      (let ([sibling (mutable-red-black-node-child sibling dir)])
        (guarded-block
         (define distant-nephew (mutable-red-black-node-child sibling inverse-dir))
         (guard (and distant-nephew (equal? (mutable-red-black-node-color distant-nephew) red)) then
           ;; fall through to case 6 (by copying the code from that case)
           (mutable-red-black-tree-rotate! tree parent dir)
           (set-mutable-red-black-node-color! sibling (mutable-red-black-node-color parent))
           (set-mutable-red-black-node-color! parent black)
           (set-mutable-red-black-node-color! distant-nephew black))
         (define close-nephew (mutable-red-black-node-child sibling inverse-dir))
         (guard (and close-nephew (equal? (mutable-red-black-node-color close-nephew) red)) then
           ;; fall through to case 5 (by copying the code from that case)
           (mutable-red-black-tree-rotate! tree sibling inverse-dir)
           (set-mutable-red-black-node-color! sibling red)
           (set-mutable-red-black-node-color! close-nephew black)
           (let* ([distant-nephew sibling]
                  [sibling close-nephew])
             ;; fall through to case 6 (by copying the code from that case)
             (mutable-red-black-tree-rotate! tree parent dir)
             (set-mutable-red-black-node-color! sibling (mutable-red-black-node-color parent))
             (set-mutable-red-black-node-color! parent black)
             (set-mutable-red-black-node-color! distant-nephew black)))
         ;; fall through to case 4 (by copying the code from that case)
         (set-mutable-red-black-node-color! sibling red)
         (set-mutable-red-black-node-color! parent black))))

    (define distant-nephew (mutable-red-black-node-child sibling inverse-dir))

    (guard (and distant-nephew (equal? (mutable-red-black-node-color distant-nephew) red)) then
      ;; Deletion case 6: distant nephew is red, sibling is black.
      (mutable-red-black-tree-rotate! tree parent dir)
      (set-mutable-red-black-node-color! sibling (mutable-red-black-node-color parent))
      (set-mutable-red-black-node-color! parent black)
      (set-mutable-red-black-node-color! distant-nephew black))

    (define close-nephew (mutable-red-black-node-child sibling dir))

    (guard (and close-nephew (equal? (mutable-red-black-node-color close-nephew) red)) then
      ;; Deletion case 5: close nephew is red, sibling and distant nephew are both black.
      (mutable-red-black-tree-rotate! tree sibling inverse-dir)
      (set-mutable-red-black-node-color! sibling red)
      (set-mutable-red-black-node-color! close-nephew black)
      (let* ([distant-nephew sibling]
             [sibling close-nephew])
        ;; fall through to case 6 (by copying the code from that case)
        (mutable-red-black-tree-rotate! tree parent dir)
        (set-mutable-red-black-node-color! sibling (mutable-red-black-node-color parent))
        (set-mutable-red-black-node-color! parent black)
        (set-mutable-red-black-node-color! distant-nephew black)))

    (guard (equal? (mutable-red-black-node-color parent) red) then
      ;; Deletion case 4: parent is red, sibling and both nephews are black
      (set-mutable-red-black-node-color! sibling red)
      (set-mutable-red-black-node-color! parent black))

    ;; Deletion cases 1 and 2: parent, sibling, and both nephews are all black.
    ;; These cases require crawling up the tree an additional level after repainting the sibling red.
    (set-mutable-red-black-node-color! sibling red)
    (let* ([node parent]
           [parent (mutable-red-black-node-parent node)])
      ;; If there is no parent, we reached the root (deletion case 2) and deletion is complete.
      (when parent
        ;; Deletion case 1: need to rebalance again at the next higher level in the tree.
        (define dir (mutable-red-black-node-parent-direction node))
        (rebalancing-loop node parent dir))))
  
  (rebalancing-loop node parent dir))


(define (mutable-red-black-tree-least-element tree)
  (define root (mutable-red-black-tree-root-node tree))
  (if root (present (mutable-red-black-node-element (mutable-red-black-node-min-child root))) absent))


(define (mutable-red-black-tree-greatest-element tree)
  (define root (mutable-red-black-tree-root-node tree))
  (if root (present (mutable-red-black-node-element (mutable-red-black-node-max-child root))) absent))


(define (mutable-red-black-tree-element-less-than tree upper-bound)
  (gap-element-before (mutable-red-black-tree-binary-search-cut tree (lower-cut upper-bound))))


(define (mutable-red-black-tree-element-greater-than tree lower-bound)
  (gap-element-before (mutable-red-black-tree-binary-search-cut tree (upper-cut lower-bound))))


(define (mutable-red-black-tree-element-at-most tree upper-bound)
  (gap-element-before (mutable-red-black-tree-binary-search-cut tree (upper-cut upper-bound))))


(define (mutable-red-black-tree-element-at-least tree lower-bound)
  (gap-element-before (mutable-red-black-tree-binary-search-cut tree (lower-cut lower-bound))))


(define (mutable-red-black-tree-generalized-binary-search tree search-function)

  (define/guard (loop [node (mutable-red-black-tree-root-node tree)]
                      [min-start-index 0]
                      [lower-element absent]
                      [upper-element absent])
    (guard node else
      (gap min-start-index lower-element upper-element))
    (define element (mutable-red-black-node-element node))
    (define left (mutable-red-black-node-left-child node))
    (define right (mutable-red-black-node-right-child node))
    (match (search-function element)
      [(== lesser)
       (define left-size (if left (mutable-red-black-node-size left) 0))
       (loop right (+ min-start-index left-size 1) (present element) upper-element)]
      [(== greater)
       (loop left min-start-index lower-element (present element))]
      [(== equivalent)
       (define left-size (if left (mutable-red-black-node-size left) 0))
       (position (+ min-start-index left-size) element)]))

  (loop))


(define (mutable-red-black-tree-binary-search tree element)
  (define cmp (mutable-red-black-tree-comparator tree))
  (mutable-red-black-tree-generalized-binary-search tree (λ (x) (compare cmp x element))))


(define (mutable-red-black-tree-binary-search-cut tree cut)
  (define cut-cmp (cut<=> (mutable-red-black-tree-comparator tree)))
  (mutable-red-black-tree-generalized-binary-search
   tree (λ (c) (compare cut-cmp (middle-cut c) cut))))


(define (mutable-red-black-node-max-child node)
  (define right (mutable-red-black-node-right-child node))
  (if right (mutable-red-black-node-max-child right) node))


(define (mutable-red-black-node-min-child node)
  (define left (mutable-red-black-node-left-child node))
  (if left (mutable-red-black-node-min-child left) node))


(define (mutable-red-black-node-swap-contents! first-node second-node)
  (define first-element (mutable-red-black-node-element first-node))
  (define second-element (mutable-red-black-node-element second-node))
  (set-mutable-red-black-node-element! first-node second-element)
  (set-mutable-red-black-node-element! second-node first-element))


(define (mutable-red-black-tree-get-node tree element)
  (define element<=> (mutable-red-black-tree-comparator tree))
  (define node (mutable-red-black-tree-root-node tree))
    
  (define/guard (loop node)
    (guard node else
      #false)
    (match (compare element<=> (mutable-red-black-node-element node) element)
      [(== equivalent) node]
      [(== lesser) (loop (mutable-red-black-node-right-child node))]
      [(== greater) (loop (mutable-red-black-node-left-child node))]))
    
  (loop node))


(define (mutable-red-black-tree-clear! tree)
  (set-mutable-red-black-tree-root-node! tree #false)
  (set-mutable-red-black-tree-size! tree 0))


(define (mutable-red-black-subtree-clear! tree element-range)
  ;; There's definitely a faster algorithm for this than just collecting all the elements into a list
  ;; and removing them one at a time. But this works fine for now, and it has the advantages of being
  ;; simple, easy to implement, and obviously correct.
  (define elements (sequence->list (in-mutable-red-black-subtree tree element-range)))
  (for ([element (in-list elements)])
    (mutable-red-black-tree-remove! tree element)))
  
  
(module+ test

  ;; Checks the red-black tree invariants: that red nodes only have black children, and that every
  ;; path from the root to a leaf must traverse the same number of black nodes.
  (define-check (mutable-red-black-tree-check-invariants tree)

    (define/guard (check-red-node-children-are-black node)
      (guard node else
        (void))
      (define left (mutable-red-black-node-left-child node))
      (define right (mutable-red-black-node-right-child node))
      (when (equal? (mutable-red-black-node-color node) red)
        (when (and left (equal? (mutable-red-black-node-color left) red))
          (with-check-info (['red-node node] ['left-child left])
            (fail-check
             "red nodes must have black child nodes, but this red node's left child is red")))
        (when (and right (equal? (mutable-red-black-node-color right) red))
          (with-check-info (['red-node node] ['right-child right])
            (fail-check
             "red nodes must have black child nodes, but this red node's right child is red"))))
      (check-red-node-children-are-black left)
      (check-red-node-children-are-black right))

    (define/guard (check-path-black-node-counts node)
      (guard node else
        0)
      (define left-count (check-path-black-node-counts (mutable-red-black-node-left-child node)))
      (define right-count (check-path-black-node-counts (mutable-red-black-node-right-child node)))
      (unless (equal? left-count right-count)
        (with-check-info (['node node]
                          ['left-black-count left-count]
                          ['right-black-count right-count])
          (fail-check
           "all paths through a red black tree must traverse the same number of black nodes")))
      (if (equal? (mutable-red-black-node-color node) black) (add1 left-count) left-count))
    
    (check-path-black-node-counts (mutable-red-black-tree-root-node tree))
    (check-red-node-children-are-black (mutable-red-black-tree-root-node tree)))
  
  (test-case (name-string mutable-red-black-tree-insert!)
      
    (test-case "insert one element into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 5)))
      
    (test-case "insert two ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 10)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 5 10)))
    
    (test-case "insert two descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 2 5)))
      
    (test-case "insert many ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
      
    (test-case "insert many descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
      
    (test-case "insert ascending and descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3)))
      
    (test-case "insert many ascending and descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
      
    (test-case "insert repeatedly ascending then descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 7)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 6)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))
      
    (test-case "insert repeatedly descending then ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 7)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 6)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))
      
    (test-case "insert many ascending elements then many descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 4)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 6)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 7)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-check-invariants tree)
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
      (mutable-red-black-tree-check-invariants tree)
      (define elements (mutable-red-black-tree-elements tree))
      (check-equal? elements '()))
      
    (test-case "clear should set size to zero"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1)
      (mutable-red-black-tree-insert! tree 2)
      (mutable-red-black-tree-insert! tree 3)
      (mutable-red-black-tree-clear! tree)
      (mutable-red-black-tree-check-invariants tree)
      (check-equal? (mutable-red-black-tree-size tree) 0)))

  (test-case "permutation test"
    (for* ([max-size (in-range 1 6)]
           [elements (in-value (range 1 (add1 max-size)))]
           [insertion-order (in-permutations elements)]
           [deletion-order (in-permutations elements)])
      (define tree (make-mutable-red-black-tree natural<=>))
      (with-check-info (['tree tree])

        (for/fold ([inserted-elements '()]
                   #:result (void))
                  ([element (in-list insertion-order)]
                   [inserted-element-count (in-naturals 1)])
          (mutable-red-black-tree-insert! tree element)
          (let ([inserted-elements (append inserted-elements (list element))])
            (with-check-info (['inserted-elements inserted-elements])

              (mutable-red-black-tree-check-invariants tree)

              (check-equal?
               (mutable-red-black-tree-elements tree)
               (sort inserted-elements <)
               "inserted elements should appear in sorted order when the tree is iterated")

              (check-equal? (mutable-red-black-tree-size tree) inserted-element-count
                            "inserting an element should increase the tree's size")
            
              inserted-elements)))

        (with-check-info (['inserted-elements insertion-order])
          (for/fold ([remaining-elements elements]
                     [deleted-elements '()]
                     #:result (void))
                    ([element (in-list deletion-order)]
                     [deleted-element-count (in-naturals 1)])
            (mutable-red-black-tree-remove! tree element)
            (let ([remaining-elements (remove element remaining-elements)]
                  [deleted-elements (append deleted-elements (list element))])
              (with-check-info (['deleted-elements deleted-elements])

                (mutable-red-black-tree-check-invariants tree)

                (check-equal? (mutable-red-black-tree-elements tree) remaining-elements
                              "removed elements should no longer appear when the tree is iterated")

                (check-equal? (mutable-red-black-tree-size tree) (- max-size deleted-element-count)
                              "removing an element should decrease the tree's size")

                (values remaining-elements deleted-elements)))))))))
