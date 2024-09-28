#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-rb-tree? (-> any/c boolean?)]
  [make-mutable-rb-tree (-> comparator? mutable-rb-tree?)]
  [mutable-rb-tree-key-comparator (-> mutable-rb-tree? comparator?)]
  [mutable-rb-tree-root-node (-> mutable-rb-tree? mutable-rb-node?)]
  [mutable-rb-tree-add-root-child! (-> mutable-rb-tree? proper-mutable-rb-node? void?)]
  [mutable-rb-tree-clear! (-> mutable-rb-tree? void?)]
  [mutable-rb-tree-size (-> mutable-rb-tree? exact-nonnegative-integer?)]
  [mutable-rb-node? (-> any/c boolean?)]
  [mutable-rb-root? (-> any/c boolean?)]
  [mutable-rb-root-node (-> mutable-rb-root? mutable-rb-node?)]
  [nil-leaf? (-> any/c boolean?)]
  [proper-mutable-rb-node? (-> any/c boolean?)]
  [make-rb-node (-> color? any/c any/c (and/c proper-mutable-rb-node? root-node?))]
  [make-red-node (-> any/c any/c (and/c proper-mutable-rb-node? root-node? red-node?))]
  [make-black-node (-> any/c any/c (and/c proper-mutable-rb-node? root-node? black-node?))]
  [black-node? (-> any/c boolean?)]
  [red-node? (-> any/c boolean?)]
  [root-node? (-> any/c boolean?)]
  [color? (-> any/c boolean?)]
  [red color?]
  [black color?]
  [direction? (-> any/c boolean?)]
  [direction-inverse (-> direction? direction?)]
  [left direction?]
  [right direction?]
  [mutable-rb-node-color (-> mutable-rb-node? color?)]
  [mutable-rb-node-parent (-> mutable-rb-node? (or/c proper-mutable-rb-node? mutable-rb-root?))]
  [mutable-rb-node-parent-direction (-> (and/c mutable-rb-node? (not/c root-node?)) direction?)]
  [mutable-rb-node-child (-> proper-mutable-rb-node? direction? mutable-rb-node?)]
  [mutable-rb-node-key (-> proper-mutable-rb-node? any/c)]
  [mutable-rb-node-value (-> proper-mutable-rb-node? any/c)]
  [mutable-rb-node-entry (-> proper-mutable-rb-node? entry?)]
  [mutable-rb-node-size (-> mutable-rb-node? exact-nonnegative-integer?)]
  [mutable-rb-node-add-child! (-> proper-mutable-rb-node? direction? proper-mutable-rb-node? void?)]
  [mutable-rb-node-remove-child! (-> proper-mutable-rb-node? direction? void?)]
  [mutable-rb-node-remove-from-parent! (-> proper-mutable-rb-node? void?)]
  [mutable-rb-node-repaint! (-> proper-mutable-rb-node? color? void?)]
  [mutable-rb-node-rotate! (-> proper-mutable-rb-node? direction? void?)]
  [mutable-rb-node-rotate-relative! (-> proper-mutable-rb-node? proper-mutable-rb-node? void?)]
  [mutable-rb-node-set-value! (-> proper-mutable-rb-node? any/c void?)]
  [mutable-rb-node-swap-contents! (-> proper-mutable-rb-node? proper-mutable-rb-node? void?)]))


(require racket/match
         racket/struct
         rebellion/base/comparator
         rebellion/collection/entry
         rebellion/private/precondition
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


(define (color? v)
  (or (equal? v red) (equal? v black)))


;; Same for the left/right enum.
(define left 'left)
(define right 'right)


(define (direction? v)
  (or (equal? v left) (equal? v right)))


(define (direction-inverse direction)
  (match direction
    [(== left) right]
    [(== right) left]))


(struct mutable-rb-tree (root key-comparator) #:constructor-name constructor:mutable-rb-tree)


(define (make-mutable-rb-tree key-comparator)
  (define root (constructor:mutable-rb-root #false))
  (set-mutable-rb-root-node! root (constructor:nil-leaf root))
  (constructor:mutable-rb-tree root key-comparator))


(define (mutable-rb-tree-root-node tree)
  (mutable-rb-root-node (mutable-rb-tree-root tree)))


(define (mutable-rb-tree-clear! tree)
  (mutable-rb-root-clear! (mutable-rb-tree-root tree)))


(define (mutable-rb-tree-size tree)
  (mutable-rb-node-size (mutable-rb-tree-root-node tree)))


(struct mutable-rb-root ([node #:mutable]) #:constructor-name constructor:mutable-rb-root)


(struct mutable-rb-node ([parent #:mutable]))


(struct nil-leaf mutable-rb-node () #:constructor-name constructor:nil-leaf)


(struct proper-mutable-rb-node mutable-rb-node
  ([left-child #:mutable]
   [right-child #:mutable]
   [color #:mutable]
   [key #:mutable]
   [value #:mutable]
   [size #:mutable])
  
  #:constructor-name constructor:proper-mutable-rb-node

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (this) (mutable-rb-node-color this))
      (λ (this)
        (define key (proper-mutable-rb-node-key this))
        (define value (proper-mutable-rb-node-value this))
        (define left (proper-mutable-rb-node-left-child this))
        (define right (proper-mutable-rb-node-right-child this))
        (append (list key)
                (if value (list value) (list))
                (if (or (proper-mutable-rb-node? left) (proper-mutable-rb-node? right))
                    (list left right)
                    (list))))))]

  #:property prop:custom-print-quotable 'never)


(define (make-rb-node color key value)
  (define node (constructor:proper-mutable-rb-node #false #false #false color key value 1))
  (set-mutable-rb-node-parent! node (constructor:mutable-rb-root node))
  (set-proper-mutable-rb-node-left-child! node (constructor:nil-leaf node))
  (set-proper-mutable-rb-node-right-child! node (constructor:nil-leaf node))
  node)


(define (make-red-node key value)
  (make-rb-node red key value))


(define (make-black-node key value)
  (make-rb-node black key value))


(define (root-node? v)
  (and (mutable-rb-node? v) (mutable-rb-root? (mutable-rb-node-parent v))))


(define (mutable-rb-node-color node)
  (if (nil-leaf? node) black (proper-mutable-rb-node-color node)))


(define (black-node? v)
  (and (mutable-rb-node? v) (equal? (mutable-rb-node-color v) black)))


(define (red-node? v)
  (and (mutable-rb-node? v) (equal? (mutable-rb-node-color v) red)))


(define (mutable-rb-node-size node)
  (if (nil-leaf? node) 0 (proper-mutable-rb-node-size node)))


(define (mutable-rb-node-repaint! node color)
  (set-proper-mutable-rb-node-color! node color))


(define (mutable-rb-node-child node direction)
  (match direction
    [(== left) (proper-mutable-rb-node-left-child node)]
    [(== right) (proper-mutable-rb-node-right-child node)]))


(define (mutable-rb-node-parent-direction node)
  (define parent (mutable-rb-node-parent node))
  (check-precondition
   (proper-mutable-rb-node? parent)
   (name mutable-rb-node-parent-direction)
   "node has no parent"
   "node" node
   "parent" parent)
  (if (equal? (proper-mutable-rb-node-left-child parent) node) left right))


(define (mutable-rb-node-key node)
  (proper-mutable-rb-node-key node))


(define (mutable-rb-node-value node)
  (proper-mutable-rb-node-value node))


(define (mutable-rb-node-set-value! node value)
  (set-proper-mutable-rb-node-value! node value))


(define (mutable-rb-node-entry node)
  (entry (proper-mutable-rb-node-key node) (proper-mutable-rb-node-value node)))


(define (mutable-rb-root-clear! root)
  (when (proper-mutable-rb-node? (mutable-rb-root-node root))
    (define node (mutable-rb-root-node root))
    (set-mutable-rb-node-parent! node (constructor:mutable-rb-root node))
    (set-mutable-rb-root-node! root (constructor:nil-leaf root))))


(define (mutable-rb-tree-add-root-child! tree child)
  (mutable-rb-root-add-child! (mutable-rb-tree-root tree) child))


(define (mutable-rb-root-add-child! root child)
  (define leaf (mutable-rb-root-node root))
  (check-precondition
   (nil-leaf? leaf)
   (name mutable-rb-root-add-child!)
   "cannot add new child to root, root already has a child"
   "root" root
   "added child" child
   "preexisting child" leaf)
  (set-mutable-rb-node-parent! leaf (constructor:mutable-rb-root leaf))
  (set-mutable-rb-root-node! root child)
  (set-mutable-rb-node-parent! child root))


(define (mutable-rb-node-add-child! node direction child)
  (define leaf (mutable-rb-node-child node direction))
  (check-precondition
   (nil-leaf? leaf)
   (name mutable-rb-node-add-child!)
   "cannot add new child, node already has a child"
   "node" node
   "child direction" direction
   "added child" child
   "preexisting child" leaf)
  (set-mutable-rb-node-parent! leaf (constructor:mutable-rb-root leaf))
  (set-mutable-rb-node-parent! child node)
  (match direction
    [(== left) (set-proper-mutable-rb-node-left-child! node child)]
    [(== right) (set-proper-mutable-rb-node-right-child! node child)])
  (mutable-rb-node-fix-size! node))


(define (mutable-rb-node-remove-child! node direction)
  (define child (mutable-rb-node-child node direction))
  (check-precondition
   (proper-mutable-rb-node? child)
   (name mutable-rb-node-remove-child!)
   "cannot remove child, node does not have a child"
   "node" node
   "child direction" direction)
  (set-mutable-rb-node-parent! child (constructor:mutable-rb-root child))
  (define leaf (constructor:nil-leaf node))
  (match direction
    [(== left) (set-proper-mutable-rb-node-left-child! node leaf)]
    [(== right) (set-proper-mutable-rb-node-right-child! node leaf)])
  (mutable-rb-node-fix-size! node))


(define (mutable-rb-node-fix-size! node)
  (define current-size (mutable-rb-node-size node))
  (define correct-size
    (+ 1
       (mutable-rb-node-size (mutable-rb-node-child node left))
       (mutable-rb-node-size (mutable-rb-node-child node right))))
  (unless (equal? current-size correct-size)
    (set-proper-mutable-rb-node-size! node correct-size)
    (define parent (mutable-rb-node-parent node))
    (unless (mutable-rb-root? parent)
      (mutable-rb-node-fix-size! parent))))


(define (mutable-rb-node-remove-from-parent! node)
  (define parent (mutable-rb-node-parent node))
  (if (proper-mutable-rb-node? parent)
      (mutable-rb-node-remove-child! parent (mutable-rb-node-parent-direction node))
      (mutable-rb-root-clear! parent)))


(define (mutable-rb-node-swap-contents! first-node second-node)
  (define first-key (proper-mutable-rb-node-key first-node))
  (define second-key (proper-mutable-rb-node-key second-node))
  (set-proper-mutable-rb-node-key! first-node second-key)
  (set-proper-mutable-rb-node-key! second-node first-key)
  (define first-value (proper-mutable-rb-node-value first-node))
  (define second-value (proper-mutable-rb-node-value second-node))
  (set-proper-mutable-rb-node-value! first-node second-value)
  (set-proper-mutable-rb-node-value! second-node first-value))


(define (mutable-rb-node-rotate! node direction)
  (define original-parent (mutable-rb-node-parent node))
  (define opposite-direction (direction-inverse direction))
  (define root? (mutable-rb-root? original-parent))
  (define original-child (mutable-rb-node-child node opposite-direction))
  (check-precondition
   (not (nil-leaf? original-child))
   (name mutable-rb-node-rotate!)
   "cannot rotate, rotating node does not have a child opposite the rotation direction"
   "node" node
   "rotation direction" direction)
  (define original-middle-grandchild
    (mutable-rb-node-child original-child direction))
  (define parent-direction (and (not root?) (mutable-rb-node-parent-direction node)))
  (if root?
      (mutable-rb-root-clear! original-parent)
      (mutable-rb-node-remove-child! original-parent parent-direction))
  (unless (nil-leaf? original-middle-grandchild)
    (mutable-rb-node-remove-child! original-child direction))
  (mutable-rb-node-remove-child! node opposite-direction)
  (if root?
      (mutable-rb-root-add-child! original-parent original-child)
      (mutable-rb-node-add-child! original-parent parent-direction original-child))
  (mutable-rb-node-add-child! original-child direction node)
  (unless (nil-leaf? original-middle-grandchild)
    (mutable-rb-node-add-child! node opposite-direction original-middle-grandchild)))


(define (mutable-rb-node-rotate-relative! parent child)
  (define rotation-direction (direction-inverse (mutable-rb-node-parent-direction child)))
  (mutable-rb-node-rotate! parent rotation-direction))


(module+ test

  (test-case (name-string make-red-node)
    (define node (make-red-node 'foo 4))
    (check-equal? (mutable-rb-node-color node) red)
    (check-equal? (mutable-rb-node-key node) 'foo)
    (check-equal? (mutable-rb-node-value node) 4)
    (check-pred nil-leaf? (mutable-rb-node-child node left))
    (check-pred nil-leaf? (mutable-rb-node-child node right))
    (check-pred mutable-rb-root? (mutable-rb-node-parent node)))

  (test-case (name-string make-black-node)
    (define node (make-black-node 'foo 4))
    (check-equal? (mutable-rb-node-color node) black)
    (check-equal? (mutable-rb-node-key node) 'foo)
    (check-equal? (mutable-rb-node-value node) 4)
    (check-pred nil-leaf? (mutable-rb-node-child node left))
    (check-pred nil-leaf? (mutable-rb-node-child node right))
    (check-pred mutable-rb-root? (mutable-rb-node-parent node))))
