#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-red-black-tree? predicate/c]
  [make-mutable-red-black-tree (-> comparator? mutable-red-black-tree?)]
  [mutable-red-black-tree-size (-> mutable-red-black-tree? natural?)]
  [mutable-red-black-tree-key-comparator (-> mutable-red-black-tree? comparator?)]
  [mutable-red-black-tree-contains-key? (-> mutable-red-black-tree? any/c boolean?)]
  [mutable-red-black-tree-least-key (-> mutable-red-black-tree? option?)]
  [mutable-red-black-tree-greatest-key (-> mutable-red-black-tree? option?)]
  [mutable-red-black-tree-key-less-than (-> mutable-red-black-tree? any/c option?)]
  [mutable-red-black-tree-key-greater-than (-> mutable-red-black-tree? any/c option?)]
  [mutable-red-black-tree-key-at-most (-> mutable-red-black-tree? any/c option?)]
  [mutable-red-black-tree-key-at-least (-> mutable-red-black-tree? any/c option?)]
  [mutable-red-black-tree-put! (-> mutable-red-black-tree? any/c any/c void?)]
  [mutable-red-black-tree-remove! (-> mutable-red-black-tree? any/c void?)]
  [mutable-red-black-tree-clear! (-> mutable-red-black-tree? void?)]
  [mutable-red-black-tree-entries (-> mutable-red-black-tree? list?)]
  [mutable-red-black-subtree-size (-> mutable-red-black-tree? range? natural?)]
  [mutable-red-black-subtree-clear! (-> mutable-red-black-tree? range? void?)]
  [in-mutable-red-black-tree
   (->* (mutable-red-black-tree?) (#:descending? boolean?) (sequence/c entry?))]
  [in-mutable-red-black-tree-keys
   (->* (mutable-red-black-tree?) (#:descending? boolean?) (sequence/c any/c))]
  [in-mutable-red-black-tree-values
   (->* (mutable-red-black-tree?) (#:descending? boolean?) (sequence/c any/c))]
  [in-mutable-red-black-subtree
   (->* (mutable-red-black-tree? range?) (#:descending? boolean?) (sequence/c entry?))]
  [in-mutable-red-black-subtree-keys
   (->* (mutable-red-black-tree? range?) (#:descending? boolean?) (sequence/c any/c))]
  [in-mutable-red-black-subtree-values
   (->* (mutable-red-black-tree? range?) (#:descending? boolean?) (sequence/c any/c))]))


(require racket/block
         racket/contract/combinator
         racket/match
         racket/math
         racket/sequence
         racket/stream
         racket/struct
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/entry
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
   [key #:mutable]
   [value #:mutable]
   [size #:mutable])
  
  #:constructor-name constructor:mutable-red-black-node

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) (name mutable-red-black-node))
      (λ (this)
        (list
         (mutable-red-black-node-color this)
         (mutable-red-black-node-key this)
         (mutable-red-black-node-value this)
         (mutable-red-black-node-left-child this)
         (mutable-red-black-node-right-child this)))))]

  #:property prop:custom-print-quotable 'never)


(define (make-mutable-red-black-node key value)
  (constructor:mutable-red-black-node #false #false #false red key value 1))


(define (mutable-red-black-node-entry node)
  (entry (mutable-red-black-node-key node) (mutable-red-black-node-value node)))


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


;; Updates the pointers in node and child to make node the parent of child.
(define (mutable-red-black-node-link-child! node child direction)
  (when child
    (set-mutable-red-black-node-child! node child direction)
    (set-mutable-red-black-node-parent! child node)
    (define added-element-count (mutable-red-black-node-size child))
    (let loop ([incorrectly-sized-node node])
      (when incorrectly-sized-node
        (define new-size (+ (mutable-red-black-node-size incorrectly-sized-node) added-element-count))
        (set-mutable-red-black-node-size! incorrectly-sized-node new-size)
        (loop (mutable-red-black-node-parent incorrectly-sized-node))))))


;; Removes the pointer from the node's parent to the node.
(define (mutable-red-black-node-unlink-parent! node)
  (when (and node (mutable-red-black-node-parent node))
    (define direction (mutable-red-black-node-parent-direction node))
    (define parent (mutable-red-black-node-parent node))
    (set-mutable-red-black-node-child! parent #false direction)
    (set-mutable-red-black-node-parent! node #false)
    (define removed-element-count (mutable-red-black-node-size node))
    (let loop ([incorrectly-sized-node parent])
      (when incorrectly-sized-node
        (define new-size
          (- (mutable-red-black-node-size incorrectly-sized-node) removed-element-count))
        (set-mutable-red-black-node-size! incorrectly-sized-node new-size)
        (loop (mutable-red-black-node-parent incorrectly-sized-node))))))


(struct mutable-red-black-tree (key-comparator [root-node #:mutable])
  #:constructor-name constructor:mutable-red-black-tree)


(define (make-mutable-red-black-tree comparator)
  (constructor:mutable-red-black-tree comparator #false))


(define (mutable-red-black-tree-size tree)
  (define root (mutable-red-black-tree-root-node tree))
  (if root (mutable-red-black-node-size root) 0))


;; Mutable-Red-Black-Node -> (Sequence Any)
(define (in-mutable-red-black-tree-node node #:descending? [descending? #false])

  (define (recur node)
    (in-mutable-red-black-tree-node node #:descending? descending?))
  
  (define entry (mutable-red-black-node-entry node))
  (define true-left (mutable-red-black-node-left-child node))
  (define true-right (mutable-red-black-node-right-child node))
  (define left (if descending? true-right true-left))
  (define right (if descending? true-left true-right))
  (cond
    [(and left right) (sequence-append (recur left) (stream entry) (recur right))]
    [left (sequence-append (recur left) (stream entry))]
    [right (sequence-append (stream entry) (recur right))]
    [else (stream entry)]))


;; Mutable-Red-Black-Tree -> (Sequence Any)
(define (in-mutable-red-black-tree tree #:descending? [descending? #false])
  (stream*
   (block
    (define root (mutable-red-black-tree-root-node tree))
    (if root (in-mutable-red-black-tree-node root #:descending? descending?) (stream)))))


(define (in-mutable-red-black-tree-keys tree #:descending? [descending? #false])
  (for/stream ([e (in-mutable-red-black-tree tree #:descending? descending?)])
    (entry-key e)))


(define (in-mutable-red-black-tree-values tree #:descending? [descending? #false])
  (for/stream ([e (in-mutable-red-black-tree tree #:descending? descending?)])
    (entry-value e)))


;; Mutable-Red-Black-Node -> (Sequence Any)
(define/guard (in-mutable-red-black-subtree-node
               node key-range #:descending? [descending? #false])

  (define (recur node)
    (in-mutable-red-black-subtree-node node key-range #:descending? descending?))
  
  (define key (mutable-red-black-node-key node))
  (define range-comparison (range-compare-to-value key-range key))
  (define true-left
    (and (not (equal? range-comparison greater)) (mutable-red-black-node-left-child node)))
  (define true-right
    (and (not (equal? range-comparison lesser)) (mutable-red-black-node-right-child node)))
  (define left (if descending? true-right true-left))
  (define right (if descending? true-left true-right))
  (define left-stream (if left (stream* (recur left)) (stream)))
  (define right-stream (if right (stream* (recur right)) (stream)))
  (define entry-stream
    (if (equal? range-comparison equivalent)
        (stream (entry key (mutable-red-black-node-value node)))
        (stream)))
  (sequence-append left-stream entry-stream right-stream))


;; Mutable-Red-Black-Tree -> (Sequence Any)
(define (in-mutable-red-black-subtree tree key-range #:descending? [descending? #false])
  (stream*
   (block
    (define root (mutable-red-black-tree-root-node tree))
    (if root
        (in-mutable-red-black-subtree-node root key-range #:descending? descending?)
        (stream)))))


(define (in-mutable-red-black-subtree-keys tree key-range #:descending? [descending? #false])
  (for/stream ([e (in-mutable-red-black-subtree tree key-range #:descending? descending?)])
    (entry-key e)))


(define (in-mutable-red-black-subtree-values tree key-range #:descending? [descending? #false])
  (for/stream ([e (in-mutable-red-black-subtree tree key-range #:descending? descending?)])
    (entry-value e)))


;; Mutable-Red-Black-Tree -> List
(define (mutable-red-black-tree-entries tree)
  (sequence->list (in-mutable-red-black-tree tree)))


(define (mutable-red-black-tree-contains-key? tree key)
  (define cmp (mutable-red-black-tree-key-comparator tree))
  (and (contract-first-order-passes? (comparator-operand-contract cmp) key)
       (mutable-red-black-tree-get-node tree key)
       #true))


(define (mutable-red-black-subtree-size tree key-range)
  (define lower (range-lower-cut key-range))
  (define upper (range-upper-cut key-range))
  (- (map-gap-index (mutable-red-black-tree-binary-search-cut tree upper))
     (map-gap-index (mutable-red-black-tree-binary-search-cut tree lower))))


(define (mutable-red-black-tree-rotate! tree subtree-root direction)

  ;; TODO: remove the size invariant debugging check
  (define size-before (mutable-red-black-tree-size tree))
  
  (define subtree-parent (mutable-red-black-node-parent subtree-root))
  (define opposite-direction (direction-inverse direction))
  (define new-subtree-root (mutable-red-black-node-child subtree-root opposite-direction))
  (unless new-subtree-root
    (raise-arguments-error
     (name mutable-red-black-tree-rotate!)
     "cannot rotate subtree, child to use as new root does not exist"))
  (define child (mutable-red-black-node-child new-subtree-root direction))
  (define subtree-parent-to-root-direction
    (and subtree-parent
         (if (equal? (mutable-red-black-node-right-child subtree-parent) subtree-root) right left)))

  ;; First we unlink the three moving nodes from their parents
  (mutable-red-black-node-unlink-parent! subtree-root)
  (mutable-red-black-node-unlink-parent! new-subtree-root)
  (mutable-red-black-node-unlink-parent! child)

  ;; Next we build the rotated subtree by linking together the moving nodes
  (mutable-red-black-node-link-child! subtree-root child opposite-direction)
  (mutable-red-black-node-link-child! new-subtree-root subtree-root direction)

  ;; Finally we plug the rotated subtree back into the original tree
  (if subtree-parent
      (mutable-red-black-node-link-child!
       subtree-parent new-subtree-root subtree-parent-to-root-direction)
      (set-mutable-red-black-tree-root-node! tree new-subtree-root))

  (define size-after (mutable-red-black-tree-size tree))
  (unless (equal? size-before size-after)
    (raise-arguments-error
     (name mutable-red-black-tree-rotate!)
     "tree rotation did not correctly preserve tree size"
     "tree" tree
     "new subtree root key" (mutable-red-black-node-key new-subtree-root))))


(define (mutable-red-black-tree-put! tree key value)
  (unless (mutable-red-black-tree-contains-key? tree key)
    (mutable-red-black-tree-insert! tree key value)))


;; Mutable-Red-Black-Tree Any -> Void
;; Inserts an element into a red-black tree. The element must not already be present in the tree.
(define/guard (mutable-red-black-tree-insert! tree key value)
  (define key<=> (mutable-red-black-tree-key-comparator tree))
  (define parent (mutable-red-black-tree-insertion-parent tree key))
  (guard parent else
    (set-mutable-red-black-tree-root-node! tree (make-mutable-red-black-node key value)))
  (define direction
    (match (compare key<=> (mutable-red-black-node-key parent) key)
      [(== equivalent)
       (raise-arguments-error
        (name mutable-red-black-tree-insert!)
        "cannot insert key, tree already contains an equivalent key")]
      [(== lesser) right]
      [(== greater) left]))
  (define node (make-mutable-red-black-node key value))
  (mutable-red-black-node-link-child! parent node direction)
  
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
(define (mutable-red-black-tree-insertion-parent tree key)
  (define key<=> (mutable-red-black-tree-key-comparator tree))
  (define node (mutable-red-black-tree-root-node tree))
  
  (define (loop node)
    (define next-direction
      (match (compare key<=> (mutable-red-black-node-key node) key)
        [(== equivalent)
         (raise-arguments-error
          (name mutable-red-black-tree-insertion-parent)
          "cannot insert key, tree already contains an equivalent key")]
        [(== lesser) right]
        [(== greater) left]))
    (define next-node (mutable-red-black-node-child node next-direction))
    (if next-node (loop next-node) node))
  
  (and node (loop node)))


(define/guard (mutable-red-black-tree-remove! tree key)
  (define node (mutable-red-black-tree-get-node tree key))
  (guard node else
    (void))
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
        (mutable-red-black-tree-clear! tree)
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


(define (mutable-red-black-tree-least-key tree)
  (define root (mutable-red-black-tree-root-node tree))
  (if root (present (mutable-red-black-node-key (mutable-red-black-node-min-child root))) absent))


(define (mutable-red-black-tree-greatest-key tree)
  (define root (mutable-red-black-tree-root-node tree))
  (if root (present (mutable-red-black-node-key (mutable-red-black-node-max-child root))) absent))


(define (mutable-red-black-tree-key-less-than tree upper-bound)
  (map-gap-key-before (mutable-red-black-tree-binary-search-cut tree (lower-cut upper-bound))))


(define (mutable-red-black-tree-key-greater-than tree lower-bound)
  (map-gap-key-after (mutable-red-black-tree-binary-search-cut tree (upper-cut lower-bound))))


(define (mutable-red-black-tree-key-at-most tree upper-bound)
  (map-gap-key-before (mutable-red-black-tree-binary-search-cut tree (upper-cut upper-bound))))


(define (mutable-red-black-tree-key-at-least tree lower-bound)
  (map-gap-key-after (mutable-red-black-tree-binary-search-cut tree (lower-cut lower-bound))))


(define (mutable-red-black-tree-generalized-binary-search tree search-function)

  (define/guard (loop [node (mutable-red-black-tree-root-node tree)]
                      [min-start-index 0]
                      [lower-entry absent]
                      [upper-entry absent])
    (guard node else
      (map-gap min-start-index lower-entry upper-entry))
    (define key (mutable-red-black-node-key node))
    (define value (mutable-red-black-node-value node))
    (define left (mutable-red-black-node-left-child node))
    (match (search-function key)
      [(== lesser)
       (define left-size (if left (mutable-red-black-node-size left) 0))
       (define right (mutable-red-black-node-right-child node))
       (loop right (+ min-start-index left-size 1) (present (entry key value)) upper-entry)]
      [(== greater)
       (loop left min-start-index lower-entry (present (entry key value)))]
      [(== equivalent)
       (define left-size (if left (mutable-red-black-node-size left) 0))
       (map-position (+ min-start-index left-size) key value)]))

  (loop))


(define (mutable-red-black-tree-binary-search tree key)
  (define key<=> (mutable-red-black-tree-key-comparator tree))
  (mutable-red-black-tree-generalized-binary-search tree (λ (x) (compare key<=> x key))))


(define (mutable-red-black-tree-binary-search-cut tree cut)
  (define cut-cmp (cut<=> (mutable-red-black-tree-key-comparator tree)))
  (mutable-red-black-tree-generalized-binary-search
   tree (λ (c) (compare cut-cmp (middle-cut c) cut))))


(define (mutable-red-black-node-max-child node)
  (define right (mutable-red-black-node-right-child node))
  (if right (mutable-red-black-node-max-child right) node))


(define (mutable-red-black-node-min-child node)
  (define left (mutable-red-black-node-left-child node))
  (if left (mutable-red-black-node-min-child left) node))


(define (mutable-red-black-node-swap-contents! first-node second-node)
  (define first-key (mutable-red-black-node-key first-node))
  (define second-key (mutable-red-black-node-key second-node))
  (set-mutable-red-black-node-key! first-node second-key)
  (set-mutable-red-black-node-key! second-node first-key)
  (define first-value (mutable-red-black-node-value first-node))
  (define second-value (mutable-red-black-node-value second-node))
  (set-mutable-red-black-node-value! first-node second-value)
  (set-mutable-red-black-node-value! second-node first-value))


(define (mutable-red-black-tree-get-node tree key)
  (define key<=> (mutable-red-black-tree-key-comparator tree))
  (define node (mutable-red-black-tree-root-node tree))
    
  (define/guard (loop node)
    (guard node else
      #false)
    (match (compare key<=> (mutable-red-black-node-key node) key)
      [(== equivalent) node]
      [(== lesser) (loop (mutable-red-black-node-right-child node))]
      [(== greater) (loop (mutable-red-black-node-left-child node))]))
    
  (loop node))


(define (mutable-red-black-tree-clear! tree)
  (set-mutable-red-black-tree-root-node! tree #false))


(define (mutable-red-black-subtree-clear! tree key-range)
  ;; There's definitely a faster algorithm for this than just collecting all the elements into a list
  ;; and removing them one at a time. But this works fine for now, and it has the advantages of being
  ;; simple, easy to implement, and obviously correct.
  (define keys (sequence->list (in-mutable-red-black-subtree-keys tree key-range)))
  (for ([key (in-list keys)])
    (mutable-red-black-tree-remove! tree key)))
  
  
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

    (define/guard (check-node-sizes node)
      (guard node else
        (void))
      (define size (mutable-red-black-node-size node))
      (define left (mutable-red-black-node-left-child node))
      (define right (mutable-red-black-node-right-child node))
      (define left-size (if left (mutable-red-black-node-size left) 0))
      (define right-size (if right (mutable-red-black-node-size right) 0))
      (unless (equal? (+ left-size right-size 1) size)
        (with-check-info (['node node]
                          ['size size]
                          ['left-size left-size]
                          ['right-size right-size])
          (fail-check "the size recorded in each node must be equal to left-size + right-size + 1")))
      (check-node-sizes left)
      (check-node-sizes right))

    (define root (mutable-red-black-tree-root-node tree))
    (check-path-black-node-counts root)
    (check-red-node-children-are-black root)
    (check-node-sizes root))
  
  (test-case (name-string mutable-red-black-tree-insert!)
      
    (test-case "insert one element into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5 'a)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries (list (entry 5 'a))))
      
    (test-case "insert two ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 10 'b)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries (list (entry 5 'a) (entry 10 'b))))
    
    (test-case "insert two descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2 'b)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries (list (entry 2 'b) (entry 5 'a))))
      
    (test-case "insert many ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2 'b)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3 'c)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4 'd)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5 'e)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries (list (entry 1 'a) (entry 2 'b) (entry 3 'c) (entry 4 'd) (entry 5 'e))))
      
    (test-case "insert many descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 5 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4 'b)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3 'c)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2 'd)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1 'e)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries (list (entry 1 'e) (entry 2 'd) (entry 3 'c) (entry 4 'b) (entry 5 'a))))
      
    (test-case "insert ascending and descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 2 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3 'b)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1 'c)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries (list (entry 1 'c) (entry 2 'a) (entry 3 'b))))
      
    (test-case "insert many ascending and descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 3 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5 'b)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1 'c)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4 'd)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2 'e)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries (list (entry 1 'c) (entry 2 'e) (entry 3 'a) (entry 4 'd) (entry 5 'b))))
      
    (test-case "insert repeatedly ascending then descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 7 'b)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2 'c)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 6 'd)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3 'e)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5 'f)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4 'g)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal?
       entries
       (list
        (entry 1 'a) (entry 2 'c) (entry 3 'e) (entry 4 'g) (entry 5 'f) (entry 6 'd) (entry 7 'b))))
      
    (test-case "insert repeatedly descending then ascending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 7 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1 'b)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 6 'c)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2 'd)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5 'e)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3 'f)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 4 'g)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal?
       entries
       (list
        (entry 1 'b) (entry 2 'd) (entry 3 'f) (entry 4 'g) (entry 5 'e) (entry 6 'c) (entry 7 'a))))
      
    (test-case "insert many ascending elements then many descending elements into empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 4 'a)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 5 'b)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 6 'c)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 7 'd)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 3 'e)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 2 'f)
      (mutable-red-black-tree-check-invariants tree)
      (mutable-red-black-tree-insert! tree 1 'g)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal?
       entries
       (list
        (entry 1 'g) (entry 2 'f) (entry 3 'e) (entry 4 'a) (entry 5 'b) (entry 6 'c) (entry 7 'd)))))
    
  (test-case (name-string mutable-red-black-tree-clear!)
      
    (test-case "clear should do nothing to an empty tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-clear! tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries '()))
      
    (test-case "clear should remove all elements from a tree"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1 'a)
      (mutable-red-black-tree-insert! tree 2 'b)
      (mutable-red-black-tree-insert! tree 3 'c)
      (mutable-red-black-tree-clear! tree)
      (mutable-red-black-tree-check-invariants tree)
      (define entries (mutable-red-black-tree-entries tree))
      (check-equal? entries '()))
      
    (test-case "clear should set size to zero"
      (define tree (make-mutable-red-black-tree natural<=>))
      (mutable-red-black-tree-insert! tree 1 'a)
      (mutable-red-black-tree-insert! tree 2 'b)
      (mutable-red-black-tree-insert! tree 3 'c)
      (mutable-red-black-tree-clear! tree)
      (mutable-red-black-tree-check-invariants tree)
      (check-equal? (mutable-red-black-tree-size tree) 0)))

  (test-case "permutation test"
    (for* ([max-size (in-range 1 6)]
           [keys (in-value (range 1 (add1 max-size)))]
           [insertion-order (in-permutations keys)]
           [deletion-order (in-permutations keys)])
      (define tree (make-mutable-red-black-tree natural<=>))
      (with-check-info (['tree tree])

        (for/fold ([inserted-keys '()]
                   #:result (void))
                  ([key (in-list insertion-order)]
                   [inserted-key-count (in-naturals 1)])
          (mutable-red-black-tree-insert! tree key #false)
          (let ([inserted-keys (append inserted-keys (list key))])
            (with-check-info (['inserted-keys inserted-keys])

              (mutable-red-black-tree-check-invariants tree)

              (check-equal?
               (map entry-key (mutable-red-black-tree-entries tree))
               (sort inserted-keys <)
               "inserted keys should appear in sorted order when the tree is iterated")

              (check-equal? (mutable-red-black-tree-size tree) inserted-key-count
                            "inserting a key should increase the tree's size")
            
              inserted-keys)))

        (with-check-info (['inserted-keys insertion-order])
          (for/fold ([remaining-keys keys]
                     [deleted-keys '()]
                     #:result (void))
                    ([key (in-list deletion-order)]
                     [deleted-key-count (in-naturals 1)])
            (mutable-red-black-tree-remove! tree key)
            (let ([remaining-keys (remove key remaining-keys)]
                  [deleted-keys (append deleted-keys (list key))])
              (with-check-info (['deleted-keys deleted-keys])

                (mutable-red-black-tree-check-invariants tree)

                (check-equal? (map entry-key (mutable-red-black-tree-entries tree)) remaining-keys
                              "removed keys should no longer appear when the tree is iterated")

                (check-equal? (mutable-red-black-tree-size tree) (- max-size deleted-key-count)
                              "removing a key should decrease the tree's size")

                (values remaining-keys deleted-keys)))))))))
