#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [persistent-red-black-tree? predicate/c]
  [in-persistent-red-black-tree
   (->* (persistent-red-black-tree?) (#:descending? boolean?) (sequence/c any/c))]
  [in-persistent-red-black-subtree
   (->* (persistent-red-black-tree? range?) (#:descending? boolean?) (sequence/c any/c))]
  [empty-persistent-red-black-tree (-> comparator? persistent-red-black-tree?)]
  [persistent-red-black-tree-size (-> persistent-red-black-tree? natural?)]
  [persistent-red-black-tree-comparator (-> persistent-red-black-tree? comparator?)]
  [persistent-red-black-tree-contains? (-> persistent-red-black-tree? any/c boolean?)]
  [persistent-red-black-tree-insert (-> persistent-red-black-tree? any/c persistent-red-black-tree?)]
  [persistent-red-black-tree-remove (-> persistent-red-black-tree? any/c persistent-red-black-tree?)]
  [persistent-red-black-tree-elements (-> persistent-red-black-tree? list?)]
  [persistent-red-black-tree-least-element (-> persistent-red-black-tree? option?)]
  [persistent-red-black-tree-greatest-element (-> persistent-red-black-tree? option?)]
  [persistent-red-black-tree-element-greater-than (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-element-less-than (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-element-at-most (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-element-at-least (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-binary-search
   (-> persistent-red-black-tree? any/c (or/c position? gap?))]
  [persistent-red-black-tree-binary-search-cut
   (-> persistent-red-black-tree? cut? (or/c position? gap?))]
  [persistent-red-black-subtree-copy
   (-> persistent-red-black-tree? range? persistent-red-black-tree?)]
  [persistent-red-black-subtree-size (-> persistent-red-black-tree? range? natural?)]
  [persistent-red-black-subtree-contains? (-> persistent-red-black-tree? range? any/c boolean?)]
  [persistent-red-black-subtree-least-element (-> persistent-red-black-tree? range? option?)]
  [persistent-red-black-subtree-greatest-element (-> persistent-red-black-tree? range? option?)]
  [persistent-red-black-subtree-element-greater-than
   (-> persistent-red-black-tree? range? any/c option?)]
  [persistent-red-black-subtree-element-less-than
   (-> persistent-red-black-tree? range? any/c option?)]
  [persistent-red-black-subtree-element-at-most (-> persistent-red-black-tree? range? any/c option?)]
  [persistent-red-black-subtree-element-at-least (-> persistent-red-black-tree? range? any/c option?)]
  [sorted-unique-sequence->persistent-red-black-tree
   (-> (sequence/c any/c) comparator? persistent-red-black-tree?)]))


(require racket/match
         racket/math
         racket/sequence
         racket/stream
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/private/vector-binary-search
         rebellion/private/cut
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------
;; Immutable persistent red-black trees (Okasaki's implementation)


;; We use constants for the red/black color enum instead of define-enum-type to avoid unnecessary
;; dependencies on other parts of Rebellion, especially cyclic dependencies. We define constants
;; instead of using the symbols directly so that typos are compile-time errors.
(define red 'red)
(define black 'black)


(struct persistent-red-black-node
  (color left-child element right-child size)
  #:constructor-name constructor:persistent-red-black-node)


(define (singleton-red-black-node element)
  (constructor:persistent-red-black-node red #false element #false 1))


(define (make-red-black-node color left element right)
  (define children-size
    (cond
      [(and left right)
       (persistent-red-black-node-size left) (persistent-red-black-node-size right)]
      [left (persistent-red-black-node-size left)]
      [right (persistent-red-black-node-size right)]
      [else 0]))
  (constructor:persistent-red-black-node color left element right (add1 children-size)))


(define (make-black-node left element right)
  (make-red-black-node black left element right))


(define (red-node? v)
  (and (persistent-red-black-node? v) (equal? (persistent-red-black-node-color v) red)))


(define (persistent-red-black-node-paint-red node)
  (struct-copy persistent-red-black-node node [color red]))


(struct persistent-red-black-tree
  (comparator root-node)
  #:guard (struct-guard/c comparator? (or/c persistent-red-black-node? #false))
  #:constructor-name constructor:persistent-red-black-tree)


(define (empty-persistent-red-black-tree comparator)
  (constructor:persistent-red-black-tree comparator #false))


(define (persistent-red-black-tree-size tree)
  (define root (persistent-red-black-tree-root-node tree))
  (if root (persistent-red-black-node-size root) 0))


(define (persistent-red-black-tree-contains? tree element)
  (define cmp (persistent-red-black-tree-comparator tree))

  (define/guard (loop [node (persistent-red-black-tree-root-node tree)])
    (guard node else
      #false)
    (match-define (persistent-red-black-node _ left node-element right _) node)
    (match (compare cmp node-element element)
      [(== lesser) (loop right)]
      [(== greater) (loop left)]
      [(== equivalent) #true]))
  
  (loop))


(define (persistent-red-black-subtree-contains? tree range element)
  (and (range-contains? range element) (persistent-red-black-tree-contains? tree element)))


(define (persistent-red-black-tree-generalized-binary-search tree search-function)

  (define/guard (loop [node (persistent-red-black-tree-root-node tree)]
                      [min-start-index 0]
                      [lower-element absent]
                      [upper-element absent])
    (guard node else
      (gap min-start-index lower-element upper-element))
    (match-define (persistent-red-black-node _ left element right _) node)
    (match (search-function element)
      [(== lesser)
       (define left-size (if left (persistent-red-black-node-size left) 0))
       (loop right (+ min-start-index left-size 1) (present element) upper-element)]
      [(== greater)
       (loop left min-start-index lower-element (present element))]
      [(== equivalent)
       (define left-size (if left (persistent-red-black-node-size left) 0))
       (position (+ min-start-index left-size) element)]))

  (loop))


(define (persistent-red-black-tree-binary-search tree element)
  (define cmp (persistent-red-black-tree-comparator tree))
  (persistent-red-black-tree-generalized-binary-search tree (λ (x) (compare cmp x element))))


(define (persistent-red-black-tree-binary-search-cut tree cut)
  (define cut-cmp (cut<=> (persistent-red-black-tree-comparator tree)))
  (persistent-red-black-tree-generalized-binary-search
   tree (λ (c) (compare cut-cmp (middle-cut c) cut))))


(define (persistent-red-black-subtree-size tree range)
  (define lower (range-lower-cut range))
  (define upper (range-upper-cut range))
  (- (gap-index (persistent-red-black-tree-binary-search-cut tree upper))
     (gap-index (persistent-red-black-tree-binary-search-cut tree lower))))


(define (persistent-red-black-tree-insert tree element)
  (define element<=> (persistent-red-black-tree-comparator tree))
  (define root (persistent-red-black-tree-root-node tree))
  
  (define/guard (loop node)
    (guard node else
      (singleton-red-black-node element))
    (define node-element (persistent-red-black-node-element node))
    (match (compare element<=> element node-element)
      [(== equivalent) node]
      
      [(== lesser)
       (define new-node
         (constructor:persistent-red-black-node
          (persistent-red-black-node-color node)
          (loop (persistent-red-black-node-left-child node))
          (persistent-red-black-node-element node)
          (persistent-red-black-node-right-child node)
          (add1 (persistent-red-black-node-size node))))
       (persistent-red-black-node-rebalance-left new-node)]
      
      [(== greater)
       (define new-node
         (constructor:persistent-red-black-node
          (persistent-red-black-node-color node)
          (persistent-red-black-node-left-child node)
          (persistent-red-black-node-element node)
          (loop (persistent-red-black-node-right-child node))
          (add1 (persistent-red-black-node-size node))))
       (persistent-red-black-node-rebalance-right new-node)]))
  
  (constructor:persistent-red-black-tree element<=> (loop root)))


(define (persistent-red-black-node-rebalance-left node)
  (match node
    
    [(persistent-red-black-node
      (== black)
      (persistent-red-black-node (== red) (? red-node? red-left) y c _)
      z
      d
      parent-size)
     (define left (persistent-red-black-node-paint-red red-left))
     (define right (make-black-node c z d))
     (constructor:persistent-red-black-node red left y right parent-size)]
    
    [(persistent-red-black-node
      (== black)
      (persistent-red-black-node (== red) a x (persistent-red-black-node (== red) b y c _) _)
      z
      d
      parent-size)
     (define left (make-black-node a x b))
     (define right (make-black-node c z d))
     (constructor:persistent-red-black-node red left y right parent-size)]
    
    [else node]))


(define (persistent-red-black-node-rebalance-right node)
  (match node
    
    [(persistent-red-black-node
      (== black)
      a
      x
      (persistent-red-black-node (== red) (persistent-red-black-node (== red) b y c _) z d _)
      parent-size)
     (define left (make-black-node a x b))
     (define right (make-black-node c z d))
     (constructor:persistent-red-black-node red left y right parent-size)]
    
    [(persistent-red-black-node
      (== black)
      a
      x
      (persistent-red-black-node (== red) b y (? red-node? red-right) _)
      parent-size)
     (define left (make-black-node a x b))
     (define right (persistent-red-black-node-paint-red red-right))
     (constructor:persistent-red-black-node red left y right parent-size)]
    
    [else node]))


(define (persistent-red-black-tree-remove tree element)
  ;; TODO
  tree)


(define (persistent-red-black-tree-elements tree)
  (sequence->list (in-persistent-red-black-tree tree)))


(define (in-persistent-red-black-tree tree #:descending? [descending? #false])
  
  (define in-node
    (if descending?
        (λ (node)
          (if node
              (sequence-append
               (in-node (persistent-red-black-node-right-child node))
               (stream (persistent-red-black-node-element node))
               (in-node (persistent-red-black-node-left-child node)))
              (stream)))
        (λ (node)
          (if node
              (sequence-append
               (in-node (persistent-red-black-node-left-child node))
               (stream (persistent-red-black-node-element node))
               (in-node (persistent-red-black-node-right-child node)))
              (stream)))))
  
  (stream* (in-node (persistent-red-black-tree-root-node tree))))


(define (in-persistent-red-black-subtree tree range #:descending? [descending? #false])

  (define/guard (in-ascending-node node)
    (guard node else
      (stream))
    (define element (persistent-red-black-node-element node))
    (match (range-compare-to-value range element)
      [(== lesser) (in-ascending-node (persistent-red-black-node-right-child node))]
      [(== greater) (in-ascending-node (persistent-red-black-node-left-child node))]
      [(== equivalent)
       (sequence-append
        (in-ascending-node (persistent-red-black-node-right-child node))
        (stream element)
        (in-ascending-node (persistent-red-black-node-left-child node)))]))

  (define/guard (in-descending-node node)
    (guard node else
      (stream))
    (define element (persistent-red-black-node-element node))
    (match (range-compare-to-value range element)
      [(== lesser) (in-descending-node (persistent-red-black-node-right-child node))]
      [(== greater) (in-descending-node (persistent-red-black-node-left-child node))]
      [(== equivalent)
       (sequence-append
        (in-descending-node (persistent-red-black-node-left-child node))
        (stream element)
        (in-descending-node (persistent-red-black-node-right-child node)))]))

  (define root (persistent-red-black-tree-root-node tree))
  (if descending? (stream* (in-descending-node root)) (stream* (in-ascending-node root))))


(define (sorted-unique-sequence->persistent-red-black-tree elements comparator)
  ;; TODO
  (empty-persistent-red-black-tree comparator))


(define/guard (persistent-red-black-tree-least-element tree)
  (define root (persistent-red-black-tree-root-node tree))
  (guard root else
    absent)
  
  (define (loop node)
    (match (persistent-red-black-node-left-child node)
      [#false (persistent-red-black-node-element node)]
      [left-child (loop left-child)]))

  (present (loop root)))


(define/guard (persistent-red-black-subtree-least-element tree range)
  (define lower (range-lower-bound range))
  (guard (equal? lower unbounded) then
    (persistent-red-black-tree-least-element tree))
  (define endpoint (range-bound-endpoint lower))
  (match (range-bound-type lower)
    [(== inclusive) (persistent-red-black-tree-element-at-least tree endpoint)]
    [(== exclusive) (persistent-red-black-tree-element-greater-than tree endpoint)]))


(define/guard (persistent-red-black-tree-greatest-element tree)
  (define root (persistent-red-black-tree-root-node tree))
  (guard root else
    absent)
  
  (define (loop node)
    (match (persistent-red-black-node-right-child node)
      [#false (persistent-red-black-node-element node)]
      [right-child (loop right-child)]))

  (present (loop root)))


(define/guard (persistent-red-black-subtree-greatest-element tree range)
  (define upper (range-upper-bound range))
  (guard (equal? upper unbounded) then
    (persistent-red-black-tree-greatest-element tree))
  (define endpoint (range-bound-endpoint upper))
  (match (range-bound-type upper)
    [(== inclusive) (persistent-red-black-tree-element-at-most tree endpoint)]
    [(== exclusive) (persistent-red-black-tree-element-less-than tree endpoint)]))


(define (persistent-red-black-tree-element-less-than tree upper-bound)
  (gap-element-before (persistent-red-black-tree-binary-search-cut tree (lower-cut upper-bound))))


(define (persistent-red-black-subtree-element-less-than tree range upper-bound)
  (match (range-compare-to-cut range (lower-cut upper-bound))
    [(== greater) (persistent-red-black-subtree-greatest-element tree range)]
    [(== lesser) absent]
    [(== equivalent)
     (define result (persistent-red-black-tree-element-less-than tree upper-bound))
     (option-filter result (λ (element) (range-contains? range element)))]))


(define (persistent-red-black-tree-element-greater-than tree lower-bound)
  (gap-element-after (persistent-red-black-tree-binary-search-cut tree (upper-cut lower-bound))))


(define (persistent-red-black-subtree-element-greater-than tree range lower-bound)
  (match (range-compare-to-cut range (upper-cut lower-bound))
    [(== lesser) (persistent-red-black-subtree-least-element tree range)]
    [(== greater) absent]
    [(== equivalent)
     (define result (persistent-red-black-tree-element-greater-than tree lower-bound))
     (option-filter result (λ (element) (range-contains? range element)))]))


(define (persistent-red-black-tree-element-at-most tree upper-bound)
  (match (persistent-red-black-tree-binary-search tree upper-bound)
    [(position _ equivalent-element) (present equivalent-element)]
    [(gap _ lesser-element _) lesser-element]))


(define (persistent-red-black-subtree-element-at-most tree range upper-bound)
  (match (range-compare-to-cut range (upper-cut upper-bound))
    [(== greater) (persistent-red-black-subtree-greatest-element tree range)]
    [(== lesser) absent]
    [(== equivalent)
     (define result (persistent-red-black-tree-element-at-most tree upper-bound))
     (option-filter result (λ (element) (range-contains? range element)))]))


(define (persistent-red-black-tree-element-at-least tree lower-bound)
  (match (persistent-red-black-tree-binary-search tree lower-bound)
    [(position _ equivalent-element) (present equivalent-element)]
    [(gap _ _ greater-element) greater-element]))


(define (persistent-red-black-subtree-element-at-least tree range lower-bound)
  (match (range-compare-to-cut range (lower-cut lower-bound))
    [(== lesser) (persistent-red-black-subtree-least-element tree range)]
    [(== greater) absent]
    [(== equivalent)
     (define result (persistent-red-black-tree-element-at-least tree lower-bound))
     (option-filter result (λ (element) (range-contains? range element)))]))


(define/guard (persistent-red-black-subtree-copy tree range)
  (guard-match (present least) (persistent-red-black-tree-least-element tree) else
    (empty-persistent-red-black-tree (persistent-red-black-tree-comparator tree)))
  (match-define (present greatest) (persistent-red-black-tree-greatest-element tree))
  (guard (and (range-contains? range least) (range-contains? range greatest)) then
    tree)
  (for/fold ([tree (empty-persistent-red-black-tree (persistent-red-black-tree-comparator tree))])
            ([element (in-persistent-red-black-subtree tree range)])
    (persistent-red-black-tree-insert tree element)))


(module+ test
  
  (define empty-tree (empty-persistent-red-black-tree natural<=>))
  (define (tree-of . elements)
    (for/fold ([tree empty-tree])
              ([element (in-list elements)])
      (persistent-red-black-tree-insert tree element)))
  
  (test-case (name-string persistent-red-black-tree-size)
    
    (test-case "empty trees"
      (check-equal? (persistent-red-black-tree-size empty-tree) 0))
    
    (test-case "singleton trees"
      (define tree (tree-of 5))
      (check-equal? (persistent-red-black-tree-size tree) 1))
    
    (test-case "trees with many elements"
      (define tree (tree-of 3 5 2 1 4))
      (check-equal? (persistent-red-black-tree-size tree) 5)))
  
  (test-case (name-string persistent-red-black-tree-insert)
    
    (test-case "insert one element into empty tree"
      (define tree (tree-of 5))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 5)))
    
    (test-case "insert two ascending elements into empty tree"
      (define tree (tree-of 5 10))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 5 10)))
    
    (test-case "insert two descending elements into empty tree"
      (define tree (tree-of 5 2))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 2 5)))
    
    (test-case "insert many ascending elements into empty tree"
      (define tree (tree-of 1 2 3 4 5))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert many descending elements into empty tree"
      (define tree (tree-of 5 4 3 2 1))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert ascending and descending elements into empty tree"
      (define tree (tree-of 2 3 1))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3)))
    
    (test-case "insert many ascending and descending elements into empty tree"
      (define tree (tree-of 3 5 1 4 2))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert repeatedly ascending then descending elements into empty tree"
      (define tree (tree-of 1 7 2 6 3 5 4))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))
    
    (test-case "insert repeatedly descending then ascending elements into empty tree"
      (define tree (tree-of 7 1 6 2 5 3 4))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))
    
    (test-case "insert many ascending elements then many descending elements into empty tree"
      (define tree (tree-of 4 5 6 7 3 2 1))
      (define elements (persistent-red-black-tree-elements tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))))
