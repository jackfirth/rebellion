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


(require racket/block
         racket/match
         racket/math
         racket/sequence
         racket/stream
         rebellion/base/comparator
         rebellion/base/option
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------
;; Immutable persistent red-black trees (Okasaki's implementation)



(struct persistent-red-black-node
  (element left-child right-child size)
  #:constructor-name constructor:persistent-red-black-node)


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
  ;; TODO
  tree)


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
      ;; TODO
      (void))
    
    (test-case "trees with many elements"
      ;; TODO
      (void))))
