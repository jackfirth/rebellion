#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutable-rb-tree-get (-> mutable-rb-tree? any/c option?)]
  [mutable-rb-tree-get-node (-> mutable-rb-tree? any/c mutable-rb-node?)]
  [mutable-rb-tree-get-entry (-> mutable-rb-tree? any/c (option/c entry?))]
  [mutable-rb-node-min-child (-> proper-mutable-rb-node? proper-mutable-rb-node?)]
  [mutable-rb-node-max-child (-> proper-mutable-rb-node? proper-mutable-rb-node?)]
  [mutable-rb-tree-contains-key? (-> mutable-rb-tree? any/c boolean?)]
  [mutable-rb-tree-least-key (-> mutable-rb-tree? option?)]
  [mutable-rb-tree-greatest-key (-> mutable-rb-tree? option?)]
  [mutable-rb-tree-key-less-than (-> mutable-rb-tree? any/c option?)]
  [mutable-rb-tree-key-greater-than (-> mutable-rb-tree? any/c option?)]
  [mutable-rb-tree-key-at-most (-> mutable-rb-tree? any/c option?)]
  [mutable-rb-tree-key-at-least (-> mutable-rb-tree? any/c option?)]
  [mutable-rb-subtree-size (-> mutable-rb-tree? range? exact-nonnegative-integer?)]))


(require racket/contract/combinator
         racket/match
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/entry
         rebellion/collection/private/mutable-red-black-tree-base
         rebellion/collection/private/vector-binary-search
         rebellion/private/cut
         rebellion/private/guarded-block)


;@----------------------------------------------------------------------------------------------------


(define (mutable-rb-tree-get tree key)
  (define node (mutable-rb-tree-get-node tree key))
  (if (nil-leaf? node) absent (present (mutable-rb-node-value node))))


(define (mutable-rb-tree-get-entry tree key)
  (define node (mutable-rb-tree-get-node tree key))
  (if (nil-leaf? node) absent (present (mutable-rb-node-entry node))))


(define (mutable-rb-tree-get-node tree key)
  (define key<=> (mutable-rb-tree-key-comparator tree))
  (let loop ([node (mutable-rb-tree-root-node tree)])
    (guarded-block
     (guard (nil-leaf? node) then
       node)
     (define node-key (mutable-rb-node-key node))
     (match (compare key<=> key node-key)
       [(== equivalent) node]
       [(== lesser) (loop (mutable-rb-node-child node left))]
       [(== greater) (loop (mutable-rb-node-child node right))]))))


(define (mutable-rb-node-min-child node)
  (let loop ([node node])
    (define child (mutable-rb-node-child node left))
    (if (nil-leaf? child) node (loop child))))


(define (mutable-rb-node-max-child node)
  (let loop ([node node])
    (define child (mutable-rb-node-child node right))
    (if (nil-leaf? child) node (loop child))))


(define (mutable-rb-tree-contains-key? tree key)
  (define cmp (mutable-rb-tree-key-comparator tree))
  (and (contract-first-order-passes? (comparator-operand-contract cmp) key)
       (proper-mutable-rb-node? (mutable-rb-tree-get-node tree key))))


(define (mutable-rb-subtree-size tree key-range)
  (define lower (range-lower-cut key-range))
  (define upper (range-upper-cut key-range))
  (- (map-gap-index (mutable-rb-tree-binary-search-cut tree upper))
     (map-gap-index (mutable-rb-tree-binary-search-cut tree lower))))


(define (mutable-rb-tree-least-key tree)
  (define root (mutable-rb-tree-root-node tree))
  (if (nil-leaf? root) absent (present (mutable-rb-node-key (mutable-rb-node-min-child root)))))


(define (mutable-rb-tree-greatest-key tree)
  (define root (mutable-rb-tree-root-node tree))
  (if (nil-leaf? root) absent (present (mutable-rb-node-key (mutable-rb-node-max-child root)))))


(define (mutable-rb-tree-key-less-than tree upper-bound)
  (map-gap-key-before (mutable-rb-tree-binary-search-cut tree (lower-cut upper-bound))))


(define (mutable-rb-tree-key-greater-than tree lower-bound)
  (map-gap-key-after (mutable-rb-tree-binary-search-cut tree (upper-cut lower-bound))))


(define (mutable-rb-tree-key-at-most tree upper-bound)
  (map-gap-key-before (mutable-rb-tree-binary-search-cut tree (upper-cut upper-bound))))


(define (mutable-rb-tree-key-at-least tree lower-bound)
  (map-gap-key-after (mutable-rb-tree-binary-search-cut tree (lower-cut lower-bound))))


(define (mutable-rb-tree-generalized-binary-search tree search-function)

  (define/guard (loop [node (mutable-rb-tree-root-node tree)]
                      [min-start-index 0]
                      [lower-entry absent]
                      [upper-entry absent])
    (guard (proper-mutable-rb-node? node) else
      (map-gap min-start-index lower-entry upper-entry))
    (define key (mutable-rb-node-key node))
    (define value (mutable-rb-node-value node))
    (define left-child (mutable-rb-node-child node left))
    (match (search-function key)
      [(== lesser)
       (define left-size (mutable-rb-node-size left-child))
       (define right-child (mutable-rb-node-child node right))
       (loop right-child (+ min-start-index left-size 1) (present (entry key value)) upper-entry)]
      [(== greater)
       (loop left-child min-start-index lower-entry (present (entry key value)))]
      [(== equivalent)
       (define left-size (mutable-rb-node-size left-child))
       (map-position (+ min-start-index left-size) key value)]))

  (loop))


(define (mutable-rb-tree-binary-search tree key)
  (define key<=> (mutable-rb-tree-key-comparator tree))
  (mutable-rb-tree-generalized-binary-search tree (λ (x) (compare key<=> x key))))


(define (mutable-rb-tree-binary-search-cut tree cut)
  (define cut-cmp (cut<=> (mutable-rb-tree-key-comparator tree)))
  (mutable-rb-tree-generalized-binary-search
   tree (λ (c) (compare cut-cmp (middle-cut c) cut))))
