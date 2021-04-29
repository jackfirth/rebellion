#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [persistent-red-black-tree? predicate/c]
  [empty-persistent-red-black-tree (-> comparator? persistent-red-black-tree?)]
  [persistent-red-black-tree-size (-> persistent-red-black-tree? natural?)]
  [persistent-red-black-tree-contains? (-> persistent-red-black-tree? any/c boolean?)]
  [persistent-red-black-tree-insert (-> persistent-red-black-tree? any/c persistent-red-black-tree?)]
  [persistent-red-black-tree-remove (-> persistent-red-black-tree? any/c persistent-red-black-tree?)]
  [persistent-red-black-tree-elements (-> persistent-red-black-tree? list?)]
  [in-persistent-red-black-tree (-> persistent-red-black-tree? (sequence/c any/c))]
  [sorted-unique-sequence->persistent-red-black-tree
   (-> (sequence/c any/c) comparator? persistent-red-black-tree?)]))


(require racket/match
         racket/math
         racket/sequence
         rebellion/base/comparator
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
  (element color left-child right-child size)
  #:constructor-name constructor:persistent-red-black-node)


(define (singleton-red-black-node element)
  (constructor:persistent-red-black-node element red #false #false 1))


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
  ;; TODO
  #false)


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
          (persistent-red-black-node-element node)
          (persistent-red-black-node-color node)
          (loop (persistent-red-black-node-left-child node))
          (persistent-red-black-node-right-child node)
          (add1 (persistent-red-black-node-size node))))
       (persistent-red-black-node-rebalance-left new-node)]
      
      [(== greater)
       (define new-node
         (constructor:persistent-red-black-node
          (persistent-red-black-node-element node)
          (persistent-red-black-node-color node)
          (persistent-red-black-node-left-child node)
          (loop (persistent-red-black-node-right-child node))
          (add1 (persistent-red-black-node-size node))))
       (persistent-red-black-node-rebalance-right new-node)]))
  
  (constructor:persistent-red-black-tree element<=> (loop root)))


(define (persistent-red-black-node-rebalance-left node)
  ;; TODO
  node)


(define (persistent-red-black-node-rebalance-right node)
  ;; TODO
  node)


(define (persistent-red-black-tree-remove tree element)
  ;; TODO
  tree)


(define (persistent-red-black-tree-elements tree)
  (sequence->list (in-persistent-red-black-tree tree)))


(define (in-persistent-red-black-tree tree)
  ;; TODO
  (list))


(define (sorted-unique-sequence->persistent-red-black-tree elements comparator)
  ;; TODO
  (empty-persistent-red-black-tree comparator))


(module+ test
  (test-case (name-string persistent-red-black-tree-size)
    
    (test-case "empty trees"
      (define tree (empty-persistent-red-black-tree natural<=>))
      (check-equal? (persistent-red-black-tree-size tree) 0))
    
    (test-case "singleton trees"
      (define tree
        (persistent-red-black-tree-insert
         (empty-persistent-red-black-tree natural<=>)
         5))
      (check-equal? (persistent-red-black-tree-size tree) 1))
    
    (test-case "trees with many elements"
      (define tree
        (persistent-red-black-tree-insert
         (persistent-red-black-tree-insert
          (persistent-red-black-tree-insert
           (persistent-red-black-tree-insert
            (persistent-red-black-tree-insert
             (empty-persistent-red-black-tree natural<=>)
             3)
            5)
           2)
          1)
         4))
      (check-equal? (persistent-red-black-tree-size tree) 5))))
