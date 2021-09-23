#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [persistent-red-black-tree? predicate/c]
  [in-persistent-red-black-tree
   (->* (persistent-red-black-tree?) (#:descending? boolean?) (sequence/c entry?))]
  [in-persistent-red-black-tree-keys
   (->* (persistent-red-black-tree?) (#:descending? boolean?) (sequence/c any/c))]
  [in-persistent-red-black-tree-values
   (->* (persistent-red-black-tree?) (#:descending? boolean?) (sequence/c any/c))]
  [in-persistent-red-black-subtree
   (->* (persistent-red-black-tree? range?) (#:descending? boolean?) (sequence/c entry?))]
  [in-persistent-red-black-subtree-keys
   (->* (persistent-red-black-tree? range?) (#:descending? boolean?) (sequence/c any/c))]
  [in-persistent-red-black-subtree-values
   (->* (persistent-red-black-tree? range?) (#:descending? boolean?) (sequence/c any/c))]
  [empty-persistent-red-black-tree (-> comparator? persistent-red-black-tree?)]
  [persistent-red-black-tree-size (-> persistent-red-black-tree? natural?)]
  [persistent-red-black-tree-comparator (-> persistent-red-black-tree? comparator?)]
  [persistent-red-black-tree-contains? (-> persistent-red-black-tree? any/c boolean?)]
  [persistent-red-black-tree-get (-> persistent-red-black-tree? any/c failure-result/c any/c)]
  [persistent-red-black-tree-get-option (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-get-entry (-> persistent-red-black-tree? any/c failure-result/c entry?)]
  [persistent-red-black-tree-insert
   (-> persistent-red-black-tree? any/c any/c persistent-red-black-tree?)]
  [persistent-red-black-tree-remove (-> persistent-red-black-tree? any/c persistent-red-black-tree?)]
  [persistent-red-black-tree-update
   (-> persistent-red-black-tree? any/c (-> any/c any/c) failure-result/c persistent-red-black-tree?)]
  [persistent-red-black-tree-keys (-> persistent-red-black-tree? list?)]
  [persistent-red-black-tree-least-key (-> persistent-red-black-tree? option?)]
  [persistent-red-black-tree-greatest-key (-> persistent-red-black-tree? option?)]
  [persistent-red-black-tree-key-greater-than (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-key-less-than (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-key-at-most (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-key-at-least (-> persistent-red-black-tree? any/c option?)]
  [persistent-red-black-tree-least-entry (-> persistent-red-black-tree? (option/c entry?))]
  [persistent-red-black-tree-greatest-entry (-> persistent-red-black-tree? (option/c entry?))]
  [persistent-red-black-tree-entry-greater-than
   (-> persistent-red-black-tree? any/c (option/c entry?))]
  [persistent-red-black-tree-entry-less-than (-> persistent-red-black-tree? any/c (option/c entry?))]
  [persistent-red-black-tree-entry-at-most (-> persistent-red-black-tree? any/c (option/c entry?))]
  [persistent-red-black-tree-entry-at-least (-> persistent-red-black-tree? any/c (option/c entry?))]
  [persistent-red-black-tree-binary-search
   (-> persistent-red-black-tree? any/c (or/c map-position? map-gap?))]
  [persistent-red-black-tree-binary-search-cut
   (-> persistent-red-black-tree? cut? (or/c map-position? map-gap?))]
  [persistent-red-black-subtree-copy
   (-> persistent-red-black-tree? range? persistent-red-black-tree?)]
  [persistent-red-black-subtree-size (-> persistent-red-black-tree? range? natural?)]
  [persistent-red-black-subtree-contains? (-> persistent-red-black-tree? range? any/c boolean?)]
  [sorted-unique-sequence->persistent-red-black-tree
   (-> (sequence/c any/c) comparator? persistent-red-black-tree?)]))


(require (for-syntax racket/base
                     syntax/parse)
         racket/block
         racket/contract/combinator
         racket/match
         racket/math
         racket/pretty
         racket/sequence
         racket/stream
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
           rackunit))


;@----------------------------------------------------------------------------------------------------
;; Immutable persistent red-black trees (Okasaki's implementation)


;; We use constants for the red/black color enum instead of define-enum-type to avoid unnecessary
;; dependencies on other parts of Rebellion, especially cyclic dependencies. We define constants
;; instead of using the symbols directly so that typos are compile-time errors.
(define red 'red)
(define black 'black)


;; To implement deletion, we allow the tree to temporarily contain "double black" nodes and leaves
;; while rebalancing the tree after removing an element. This approach is based on the one outlined in
;; the "Deletion: The curse of the red-black tree" Functional Pearl paper. Link below:
;; https://matt.might.net/papers/germane2014deletion.pdf
(define double-black 'double-black)


(define black-leaf 'black-leaf)
(define double-black-leaf 'double-black-leaf)


(struct persistent-red-black-node
  (color left-child key value right-child size)
  #:constructor-name constructor:persistent-red-black-node)


(define (singleton-red-black-node key value)
  (constructor:persistent-red-black-node red black-leaf key value black-leaf 1))


(define (make-red-black-node color left key value right)
  (define children-size
    (cond
      [(and (persistent-red-black-node? left) (persistent-red-black-node? right))
       (+ (persistent-red-black-node-size left) (persistent-red-black-node-size right))]
      [(persistent-red-black-node? left) (persistent-red-black-node-size left)]
      [(persistent-red-black-node? right) (persistent-red-black-node-size right)]
      [else 0]))
  (constructor:persistent-red-black-node color left key value right (add1 children-size)))


(define (make-red-node left key value right)
  (make-red-black-node red left key value right))


(define (make-black-node left key value right)
  (make-red-black-node black left key value right))


(define (make-double-black-node left key value right)
  (make-red-black-node double-black left key value right))


(define-match-expander red-node
  (syntax-parser
    [(_ left key value right size)
     #'(persistent-red-black-node (== red) left key value right size)])
  (make-rename-transformer #'make-red-node))


(define-match-expander black-node
  (syntax-parser
    [(_ left key value right size)
     #'(persistent-red-black-node (== black) left key value right size)])
  (make-rename-transformer #'make-black-node))


(define-match-expander double-black-node
  (syntax-parser
    [(_ left key value right size)
     #'(persistent-red-black-node (== double-black) left key value right size)])
  (make-rename-transformer #'make-double-black-node))


(define (red-node? v)
  (and (persistent-red-black-node? v) (equal? (persistent-red-black-node-color v) red)))


(define (black-node? v)
  (or (equal? v black-leaf)
      (and (persistent-red-black-node? v) (equal? (persistent-red-black-node-color v) black))))


(define (double-black-node? v)
  (or (equal? v double-black-leaf)
      (and (persistent-red-black-node? v) (equal? (persistent-red-black-node-color v) double-black))))


(define (persistent-red-black-node-entry node)
  (entry (persistent-red-black-node-key node) (persistent-red-black-node-value node)))


(struct persistent-red-black-tree
  (comparator root-node)
  #:guard (struct-guard/c comparator? (or/c persistent-red-black-node? black-leaf))
  #:constructor-name constructor:persistent-red-black-tree)


;; Construction


(define (empty-persistent-red-black-tree comparator)
  (constructor:persistent-red-black-tree comparator black-leaf))


(define (sorted-unique-sequence->persistent-red-black-tree elements comparator)
  ;; TODO
  (empty-persistent-red-black-tree comparator))


(define/guard (persistent-red-black-subtree-copy tree range)
  (guard-match (present least) (persistent-red-black-tree-least-key tree) else
    (empty-persistent-red-black-tree (persistent-red-black-tree-comparator tree)))
  (match-define (present greatest) (persistent-red-black-tree-greatest-key tree))
  (guard (and (range-contains? range least) (range-contains? range greatest)) then
    tree)
  (for/fold ([tree (empty-persistent-red-black-tree (persistent-red-black-tree-comparator tree))])
            ([element (in-persistent-red-black-subtree tree range)])
    (persistent-red-black-tree-insert tree element)))


;; Iteration


(define (in-persistent-red-black-tree tree #:descending? [descending? #false])
  
  (define in-node
    (if descending?
        (λ (node)
          (if (persistent-red-black-node? node)
              (sequence-append
               (in-node (persistent-red-black-node-right-child node))
               (stream (persistent-red-black-node-entry node))
               (in-node (persistent-red-black-node-left-child node)))
              (stream)))
        (λ (node)
          (if (persistent-red-black-node? node)
              (sequence-append
               (in-node (persistent-red-black-node-left-child node))
               (stream (persistent-red-black-node-entry node))
               (in-node (persistent-red-black-node-right-child node)))
              (stream)))))
  
  (stream* (in-node (persistent-red-black-tree-root-node tree))))


(define (in-persistent-red-black-tree-keys tree #:descending? [descending? #false])
  (for/stream ([e (in-persistent-red-black-tree tree #:descending? descending?)])
    (entry-key e)))


(define (in-persistent-red-black-tree-values tree #:descending? [descending? #false])
  (for/stream ([e (in-persistent-red-black-tree tree #:descending? descending?)])
    (entry-value e)))


(define/guard (in-persistent-red-black-subtree-node
               node key-range #:descending? [descending? #false])

  (guard (persistent-red-black-node? node) else
    (stream))

  (define (recur node)
    (in-persistent-red-black-subtree-node node key-range #:descending? descending?))
  
  (define key (persistent-red-black-node-key node))
  (define range-comparison (range-compare-to-value key-range key))
  (define true-left
    (and (not (equal? range-comparison greater)) (persistent-red-black-node-left-child node)))
  (define true-right
    (and (not (equal? range-comparison lesser)) (persistent-red-black-node-right-child node)))
  (define left (if descending? true-right true-left))
  (define right (if descending? true-left true-right))
  (define left-stream (if left (stream* (recur left)) (stream)))
  (define right-stream (if right (stream* (recur right)) (stream)))
  (define entry-stream
    (if (equal? range-comparison equivalent)
        (stream (entry key (persistent-red-black-node-value node)))
        (stream)))
  (sequence-append left-stream entry-stream right-stream))


(define (in-persistent-red-black-subtree tree key-range #:descending? [descending? #false])
  (define root (persistent-red-black-tree-root-node tree))
  (in-persistent-red-black-subtree-node root key-range #:descending? descending?))


(define (in-persistent-red-black-subtree-keys tree key-range #:descending? [descending? #false])
  (for/stream ([e (in-persistent-red-black-subtree tree key-range #:descending? descending?)])
    (entry-key e)))


(define (in-persistent-red-black-subtree-values tree key-range #:descending? [descending? #false])
  (for/stream ([e (in-persistent-red-black-subtree tree key-range #:descending? descending?)])
    (entry-value e)))


;; Queries and searching


(define (persistent-red-black-tree-size tree)
  (define root (persistent-red-black-tree-root-node tree))
  (if (persistent-red-black-node? root) (persistent-red-black-node-size root) 0))


(define/guard (persistent-red-black-tree-contains? tree key)
  (define cmp (persistent-red-black-tree-comparator tree))

  (define/guard (loop [node (persistent-red-black-tree-root-node tree)])
    (guard (persistent-red-black-node? node) else
      #false)
    (match-define (persistent-red-black-node _ left node-key _ right _) node)
    (match (compare cmp node-key key)
      [(== lesser) (loop right)]
      [(== greater) (loop left)]
      [(== equivalent) #true]))
  
  (and (contract-first-order-passes? (comparator-operand-contract cmp) key) (loop)))


(define (persistent-red-black-subtree-contains? tree range key)
  (and (range-contains? range key) (persistent-red-black-tree-contains? tree key)))


(define/guard (persistent-red-black-tree-get tree key failure-result)
  (define cmp (persistent-red-black-tree-comparator tree))

  (define/guard (loop [node (persistent-red-black-tree-root-node tree)])
    (guard (persistent-red-black-node? node) else
      (if (procedure? failure-result) (failure-result) failure-result))
    (match-define (persistent-red-black-node _ left node-key value right _) node)
    (match (compare cmp node-key key)
      [(== lesser) (loop right)]
      [(== greater) (loop left)]
      [(== equivalent) value]))

  (loop
   (and (contract-first-order-passes? (comparator-operand-contract cmp) key)
        (persistent-red-black-tree-root-node tree))))


(define/guard (persistent-red-black-tree-get-option tree key)
  (define cmp (persistent-red-black-tree-comparator tree))

  (define/guard (loop [node (persistent-red-black-tree-root-node tree)])
    (guard (persistent-red-black-node? node) else
      absent)
    (match-define (persistent-red-black-node _ left node-key value right _) node)
    (match (compare cmp node-key key)
      [(== lesser) (loop right)]
      [(== greater) (loop left)]
      [(== equivalent) (present value)]))

  (loop
   (and (contract-first-order-passes? (comparator-operand-contract cmp) key)
        (persistent-red-black-tree-root-node tree))))


(define (persistent-red-black-tree-get-entry tree key failure-result)
  (entry key (persistent-red-black-tree-get tree key failure-result)))


(define (persistent-red-black-tree-update tree key updater failure-result)
  (define key<=> (persistent-red-black-tree-comparator tree))
  (define root (persistent-red-black-tree-root-node tree))
  
  (define/guard (loop node)
    (guard (persistent-red-black-node? node) else
      (define value (if (procedure? failure-result) (failure-result) failure-result))
      (singleton-red-black-node key (updater value)))
    (define node-element (persistent-red-black-node-key node))
    (match (compare key<=> key node-element)
      [(== equivalent)
       (make-red-black-node
        (persistent-red-black-node-color node)
        (persistent-red-black-node-left-child node)
        (persistent-red-black-node-key node)
        (updater (persistent-red-black-node-value node))
        (persistent-red-black-node-right-child node))]
      
      [(== lesser)
       (define new-node
         (make-red-black-node
          (persistent-red-black-node-color node)
          (loop (persistent-red-black-node-left-child node))
          (persistent-red-black-node-key node)
          (persistent-red-black-node-value node)
          (persistent-red-black-node-right-child node)))
       (balance new-node)]
      
      [(== greater)
       (define new-node
         (make-red-black-node
          (persistent-red-black-node-color node)
          (persistent-red-black-node-left-child node)
          (persistent-red-black-node-key node)
          (persistent-red-black-node-value node)
          (loop (persistent-red-black-node-right-child node))))
       (balance new-node)]))
  
  (constructor:persistent-red-black-tree key<=> (loop (blacken root))))


(define (persistent-red-black-tree-generalized-binary-search tree search-function)

  (define/guard (loop [node (persistent-red-black-tree-root-node tree)]
                      [min-start-index 0]
                      [lower-entry absent]
                      [upper-entry absent])
    (guard-match (persistent-red-black-node _ left key value right _) node else
      (map-gap min-start-index lower-entry upper-entry))
    (match (search-function key)
      [(== lesser)
       (define left-size
         (if (persistent-red-black-node? left) (persistent-red-black-node-size left) 0))
       (loop right (+ min-start-index left-size 1) (present (entry key value)) upper-entry)]
      [(== greater)
       (loop left min-start-index lower-entry (present (entry key value)))]
      [(== equivalent)
       (define left-size
         (if (persistent-red-black-node? left) (persistent-red-black-node-size left) 0))
       (map-position (+ min-start-index left-size) key value)]))

  (loop))


(define (persistent-red-black-tree-binary-search tree key)
  (define cmp (persistent-red-black-tree-comparator tree))
  (persistent-red-black-tree-generalized-binary-search tree (λ (x) (compare cmp x key))))


(define (persistent-red-black-tree-binary-search-cut tree cut)
  (define cut-cmp (cut<=> (persistent-red-black-tree-comparator tree)))
  (persistent-red-black-tree-generalized-binary-search
   tree (λ (c) (compare cut-cmp (middle-cut c) cut))))


(define (persistent-red-black-subtree-size tree range)
  (define lower (range-lower-cut range))
  (define upper (range-upper-cut range))
  (- (map-gap-index (persistent-red-black-tree-binary-search-cut tree upper))
     (map-gap-index (persistent-red-black-tree-binary-search-cut tree lower))))


(define (persistent-red-black-tree-keys tree)
  (sequence->list (in-persistent-red-black-tree-keys tree)))


(define/guard (persistent-red-black-tree-least-key tree)
  (define root (persistent-red-black-tree-root-node tree))
  (guard (persistent-red-black-node? root) else
    absent)
  
  (define (loop node)
    (match (persistent-red-black-node-left-child node)
      [(== black-leaf) (persistent-red-black-node-key node)]
      [left-child (loop left-child)]))

  (present (loop root)))


(define/guard (persistent-red-black-tree-least-entry tree)
  (define root (persistent-red-black-tree-root-node tree))
  (guard (persistent-red-black-node? root) else
    absent)
  
  (define (loop node)
    (match (persistent-red-black-node-left-child node)
      [(== black-leaf)
       (entry (persistent-red-black-node-key node) (persistent-red-black-node-value node))]
      [left-child (loop left-child)]))

  (present (loop root)))


(define/guard (persistent-red-black-tree-greatest-key tree)
  (define root (persistent-red-black-tree-root-node tree))
  (guard (persistent-red-black-node? root) else
    absent)
  
  (define (loop node)
    (match (persistent-red-black-node-right-child node)
      [(== black-leaf) (persistent-red-black-node-key node)]
      [right-child (loop right-child)]))

  (present (loop root)))


(define/guard (persistent-red-black-tree-greatest-entry tree)
  (define root (persistent-red-black-tree-root-node tree))
  (guard (persistent-red-black-node? root) else
    absent)
  
  (define (loop node)
    (match (persistent-red-black-node-right-child node)
      [(== black-leaf)
       (entry (persistent-red-black-node-key node) (persistent-red-black-node-value node))]
      [right-child (loop right-child)]))

  (present (loop root)))


(define (persistent-red-black-tree-entry-less-than tree upper-bound)
  (map-gap-entry-before (persistent-red-black-tree-binary-search-cut tree (lower-cut upper-bound))))


(define (persistent-red-black-tree-entry-greater-than tree lower-bound)
  (map-gap-entry-after (persistent-red-black-tree-binary-search-cut tree (upper-cut lower-bound))))


(define (persistent-red-black-tree-entry-at-most tree upper-bound)
  (match (persistent-red-black-tree-binary-search tree upper-bound)
    [(map-position _ equivalent-key value) (present (entry equivalent-key value))]
    [(map-gap _ lesser-entry _) lesser-entry]))


(define (persistent-red-black-tree-entry-at-least tree lower-bound)
  (match (persistent-red-black-tree-binary-search tree lower-bound)
    [(map-position _ equivalent-key value) (present (entry equivalent-key value))]
    [(map-gap _ _ greater-entry) greater-entry]))


(define (persistent-red-black-tree-key-less-than tree upper-bound)
  (option-map (persistent-red-black-tree-entry-less-than tree upper-bound) entry-key))


(define (persistent-red-black-tree-key-greater-than tree lower-bound)
  (option-map (persistent-red-black-tree-entry-greater-than tree lower-bound) entry-key))


(define (persistent-red-black-tree-key-at-most tree upper-bound)
  (option-map (persistent-red-black-tree-entry-at-most tree upper-bound) entry-key))


(define (persistent-red-black-tree-key-at-least tree lower-bound)
  (option-map (persistent-red-black-tree-entry-at-least tree lower-bound) entry-key))


;; Modification


(define (persistent-red-black-tree-insert tree key value)
  (define key<=> (persistent-red-black-tree-comparator tree))
  (define root (persistent-red-black-tree-root-node tree))
  
  (define/guard (loop node)
    (guard (persistent-red-black-node? node) else
      (singleton-red-black-node key value))
    (define node-element (persistent-red-black-node-key node))
    (match (compare key<=> key node-element)
      [(== equivalent) node]
      
      [(== lesser)
       (define new-node
         (make-red-black-node
          (persistent-red-black-node-color node)
          (loop (persistent-red-black-node-left-child node))
          (persistent-red-black-node-key node)
          (persistent-red-black-node-value node)
          (persistent-red-black-node-right-child node)))
       (balance new-node)]
      
      [(== greater)
       (define new-node
         (make-red-black-node
          (persistent-red-black-node-color node)
          (persistent-red-black-node-left-child node)
          (persistent-red-black-node-key node)
          (persistent-red-black-node-value node)
          (loop (persistent-red-black-node-right-child node))))
       (balance new-node)]))
  
  (constructor:persistent-red-black-tree key<=> (loop (blacken root))))


(define (persistent-red-black-tree-remove tree key)
  (define cmp (persistent-red-black-tree-comparator tree))
  (define (remove node)
    (match node

      [(== black-leaf) black-leaf]

      [(red-node (== black-leaf) x _ (== black-leaf) _)
       #:when (compare-infix cmp x == key)
       black-leaf]

      [(black-node (== black-leaf) x _ (== black-leaf) _)
       #:when (compare-infix cmp x == key)
       double-black-leaf]

      [(black-node (red-node left x xv right _) y _ (== black-leaf) _)
       #:when (compare-infix cmp y == key)
       (black-node left x xv right)]

      [(black-node (== black-leaf) x _ (red-node left y yv right _) _)
       #:when (compare-infix cmp x == key)
       (black-node left y yv right)]

      [(persistent-red-black-node color left x v right size)
       (rotate
        (match (compare cmp key x)
          [(== lesser) (make-red-black-node color (remove left) x v right)]
          [(== greater) (make-red-black-node color left x v (remove right))]
          [(== equivalent)
           (define-values (new-x new-v new-right) (min/delete right))
           (make-red-black-node color left new-x new-v new-right)]))]))

  (define new-root (remove (redden (persistent-red-black-tree-root-node tree))))
  (constructor:persistent-red-black-tree cmp new-root))


(define (redden node)
  (match node
    [(black-node (? black-node? left) x v (? black-node? right) _)
     (red-node left x v right)]
    [_ node]))


(define (blacken node)
  (match node
    [(red-node left x v right _)
     (black-node left x v right)]
    [_ node]))


(define (min/delete node)
  (match node
    [(red-node (== black-leaf) x xv (== black-leaf) _) (values x xv black-leaf)]
    [(black-node (== black-leaf) x xv (== black-leaf) _) (values x xv double-black-leaf)]
    [(black-node (== black-leaf) x xv (red-node left y yv right _) _)
     (values x xv (black-node left y yv right))]
    [(persistent-red-black-node c left x xv right _)
     (define-values (y yv new-left) (min/delete left))
     (values y yv (rotate (make-red-black-node c new-left x xv right)))]))


(define (balance node)
  (match node
    [(or (black-node (red-node (red-node a x xv b _) y yv c _) z zv d _)
         (black-node (red-node a x xv (red-node b y yv c _) _) z zv d _)
         (black-node a x xv (red-node (red-node b y yv c _) z zv d _) _)
         (black-node a x xv (red-node b y yv (red-node c z zv d _) _) _))
     (red-node (black-node a x xv b) y yv (black-node c z zv d))]
    [(or (double-black-node (red-node a x xv (red-node b y yv c _) _) z zv d _)
         (double-black-node a x xv (red-node (red-node b y yv c _) z zv d _) _))
     (black-node (black-node a x xv b) y yv (black-node c z zv d))]
    [t t]))


(define (rotate node)
  (match node

    [(red-node (? double-black-node? a-x-b) y yv (black-node c z zv d _) _)
     (balance (black-node (red-node (remove-double-black a-x-b) y yv c) z zv d))]
    [(red-node (black-node a x xv b _) y yv (? double-black-node? c-z-d) _)
     (balance (black-node a x xv (red-node b y yv (remove-double-black c-z-d))))]
    
    [(black-node (? double-black-node? a-x-b) y yv (black-node c z zv d _) _)
     (balance (double-black-node (red-node (remove-double-black a-x-b) y yv c) z zv d))]
    [(black-node (black-node a x xv b _) y yv (? double-black-node? c-z-d) _)
     (balance (double-black-node a x xv (red-node b y yv (remove-double-black c-z-d))))]
    
    [(black-node (? double-black-node? a-w-b) x xv (red-node (black-node c y yv d _) z zv e _) _)
     (black-node (balance (black-node (red-node (remove-double-black a-w-b) x xv c) y yv d)) z zv e)]
    [(black-node (red-node a w wv (black-node b x xv c _) _) y yv (? double-black-node? d-z-e) _)
     (black-node a w wv (balance (black-node b x xv (red-node c y yv (remove-double-black d-z-e)))))]
    
    [t t]))


(define (remove-double-black node)
  (match node
    [(== double-black-leaf) black-leaf]
    [(double-black-node a x xv b _) (black-node a x xv b)]))


(module+ test
  
  (define empty-tree (empty-persistent-red-black-tree natural<=>))

  (define (tree-of . elements)
    (for/fold ([tree empty-tree])
              ([element (in-list elements)])
      (persistent-red-black-tree-insert tree element #false)))

  (define (remove-all tree . keys)
    (for/fold ([tree tree])
              ([element (in-list keys)])
      (persistent-red-black-tree-remove tree element)))
  
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
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 5)))
    
    (test-case "insert two ascending elements into empty tree"
      (define tree (tree-of 5 10))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 5 10)))
    
    (test-case "insert two descending elements into empty tree"
      (define tree (tree-of 5 2))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 2 5)))
    
    (test-case "insert many ascending elements into empty tree"
      (define tree (tree-of 1 2 3 4 5))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert many descending elements into empty tree"
      (define tree (tree-of 5 4 3 2 1))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert ascending and descending elements into empty tree"
      (define tree (tree-of 2 3 1))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 3)))
    
    (test-case "insert many ascending and descending elements into empty tree"
      (define tree (tree-of 3 5 1 4 2))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 3 4 5)))
    
    (test-case "insert repeatedly ascending then descending elements into empty tree"
      (define tree (tree-of 1 7 2 6 3 5 4))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))
    
    (test-case "insert repeatedly descending then ascending elements into empty tree"
      (define tree (tree-of 7 1 6 2 5 3 4))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 3 4 5 6 7)))
    
    (test-case "insert many ascending elements then many descending elements into empty tree"
      (define tree (tree-of 4 5 6 7 3 2 1))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 3 4 5 6 7))))

  (test-case (name-string persistent-red-black-tree-remove)

    (test-case "remove from empty tree"
      (define tree (persistent-red-black-tree-remove (tree-of) 1))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list)))

    (test-case "remove contained from singleton tree"
      (define tree (persistent-red-black-tree-remove (tree-of 1) 1))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list)))

    (test-case "remove non-contained from singleton tree"
      (define tree (persistent-red-black-tree-remove (tree-of 1) 2))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1)))

    (test-case "remove min from tree with many elements"
      (define tree (persistent-red-black-tree-remove (tree-of 1 2 3 4 5) 1))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 2 3 4 5)))

    (test-case "remove max from tree with many elements"
      (define tree (persistent-red-black-tree-remove (tree-of 1 2 3 4 5) 5))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 3 4)))

    (test-case "remove middle from tree with many elements"
      (define tree (persistent-red-black-tree-remove (tree-of 1 2 3 4 5) 3))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2 4 5)))

    (test-case "remove lower half from tree with many elements in ascending order"
      (define tree (remove-all (tree-of 1 2 3 4 5) 1 2 3))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 4 5)))

    (test-case "remove lower half from tree with many elements in descending order"
      (define tree (remove-all (tree-of 1 2 3 4 5) 3 2 1))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 4 5)))

    (test-case "remove lower half from tree with many elements in alternating order"
      (define tree (remove-all (tree-of 1 2 3 4 5) 1 3 2))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 4 5)))

    (test-case "remove upper half from tree with many elements in ascending order"
      (define tree (remove-all (tree-of 1 2 3 4 5) 3 4 5))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2)))

    (test-case "remove upper half from tree with many elements in descending order"
      (define tree (remove-all (tree-of 1 2 3 4 5) 5 4 3))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2)))

    (test-case "remove upper half from tree with many elements in alternating order"
      (define tree (remove-all (tree-of 1 2 3 4 5) 3 5 4))
      (define elements (persistent-red-black-tree-keys tree))
      (check-equal? elements (list 1 2)))))
