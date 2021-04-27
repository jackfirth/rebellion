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


(struct persistent-red-black-tree
  (comparator)
  #:constructor-name constructor:persistent-red-black-tree)


(define (empty-persistent-red-black-tree comparator)
  (constructor:persistent-red-black-tree comparator))


(define (persistent-red-black-tree-size tree)
  ;; TODO
  0)


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
