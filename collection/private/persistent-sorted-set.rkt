#lang racket/base


(require racket/contract/base)


(module+ private-for-rebellion-only
  (provide
   (contract-out
    [empty-sorted-set (-> comparator? immutable-sorted-set?)]
    [make-persistent-sorted-set
     (-> (sequence/c any/c) #:comparator comparator? immutable-sorted-set?)]
    [make-persistent-sorted-set-from-sorted (-> sorted-set? immutable-sorted-set?)])))


(require racket/generic
         racket/sequence
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/private/persistent-red-black-tree
         rebellion/collection/private/reversed-sorted-set
         rebellion/collection/private/sorted-set-interface
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only)
         rebellion/collection/private/sorted-subset
         rebellion/private/guarded-block
         rebellion/private/sequence-empty)


;@----------------------------------------------------------------------------------------------------


;; We define a specialized implementation of the empty set for speed. It's included in this module so
;; that the empty set implementation can switch to the persistent implementation when elements are
;; added to it. The persistent implementation also depends on the empty implementation, since it can
;; switch back to it if an empty subset of the persistent implementation is selected.
(struct empty-sorted-set abstract-immutable-sorted-set (comparator)

  #:methods gen:sorted-set

  [(define (in-sorted-set this #:descending? [descending? #false])
     (in-list '()))

   (define (sorted-set-empty? this)
     #true)

   (define (sorted-set-size this)
     0)

   (define (sorted-set-comparator this)
     (empty-sorted-set-comparator this))

   (define (sorted-set-contains? this value)
     #false)

   (define (sorted-set-contains-any? this values)
     #false)

   (define (sorted-set-contains-all? this values)
     (sequence-empty? values))

   (define (sorted-set-contains-none? this values)
     #true)

   (define (sorted-set-least-element this)
     absent)

   (define (sorted-set-greatest-element this)
     absent)

   (define (sorted-set-element-less-than this upper-bound)
     absent)

   (define (sorted-set-element-greater-than this lower-bound)
     absent)

   (define (sorted-set-element-at-most this upper-bound)
     absent)

   (define (sorted-set-element-at-least this lower-bound)
     absent)

   (define (sorted-subset this element-range)
     this)

   (define (sorted-set-reverse this)
     (empty-sorted-set (comparator-reverse (empty-sorted-set-comparator this))))]

  #:methods gen:immutable-sorted-set

  [(define (sorted-set-add this element)
     (define comparator (empty-sorted-set-comparator this))
     (make-persistent-sorted-set (list element) #:comparator comparator))

   (define (sorted-set-add-all this elements)
     (define comparator (empty-sorted-set-comparator this))
     (make-persistent-sorted-set elements #:comparator comparator))

   (define (sorted-set-remove this element)
     this)

   (define (sorted-set-remove-all this elements)
     this)])


(struct persistent-sorted-set abstract-immutable-sorted-set (tree)

  #:constructor-name constructor:persistent-sorted-set

  #:methods gen:sorted-set

  [(define (in-sorted-set this #:descending? [descending? #false])
     (in-persistent-red-black-tree (persistent-sorted-set-tree this) #:descending? descending?))

   (define (sorted-set-size this)
     (persistent-red-black-tree-size (persistent-sorted-set-tree this)))

   (define (sorted-set-comparator this)
     (persistent-red-black-tree-comparator (persistent-sorted-set-tree this)))

   (define (sorted-set-contains? this value)
     (persistent-red-black-tree-contains? (persistent-sorted-set-tree this) value))

   (define (sorted-set-least-element this)
     (persistent-red-black-tree-least-element (persistent-sorted-set-tree this)))

   (define (sorted-set-greatest-element this)
     (persistent-red-black-tree-greatest-element (persistent-sorted-set-tree this)))

   (define (sorted-set-element-less-than this upper-bound)
     (persistent-red-black-tree-element-less-than (persistent-sorted-set-tree this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (persistent-red-black-tree-element-greater-than (persistent-sorted-set-tree this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (persistent-red-black-tree-element-at-most (persistent-sorted-set-tree this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (persistent-red-black-tree-element-at-least (persistent-sorted-set-tree this) lower-bound))

   (define (sorted-subset this element-range)
     (constructor:persistent-sorted-subset (persistent-sorted-set-tree this) element-range))

   (define (sorted-set-reverse this)
     (make-reversed-immutable-sorted-set this))]

  #:methods gen:immutable-sorted-set

  [(define (sorted-set-add this element)
     (define tree (persistent-sorted-set-tree this))
     (constructor:persistent-sorted-set (persistent-red-black-tree-insert tree element)))

   (define (sorted-set-remove this element)
     (define tree (persistent-sorted-set-tree this))
     (constructor:persistent-sorted-set (persistent-red-black-tree-remove tree element)))])


(struct persistent-sorted-subset abstract-immutable-sorted-set (delegate element-range)

  #:constructor-name constructor:persistent-sorted-subset

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (persistent-sorted-subset-delegate this))

   (define (get-tree this)
     (persistent-sorted-set-tree (get-delegate this)))

   (define (get-range this)
     (persistent-sorted-subset-element-range this))

   (define/generic generic-sorted-set-comparator sorted-set-comparator)
   
   (define (in-sorted-set this #:descending? [descending? #false])
     (in-persistent-red-black-subtree (get-tree this) (get-range this) #:descending? descending?))

   (define (sorted-set-size this)
     (persistent-red-black-subtree-size (get-tree this) (get-range this)))

   (define (sorted-set-comparator this)
     (generic-sorted-set-comparator (get-delegate this)))

   (define (sorted-set-contains? this value)
     (persistent-red-black-subtree-contains? (get-tree this) (get-range this) value))

   (define/guard (sorted-set-least-element this)
     (sorted-subset-least-element (get-delegate this) (get-range this)))

   (define/guard (sorted-set-greatest-element this)
     (sorted-subset-greatest-element (get-delegate this) (get-range this)))

   (define/guard (sorted-set-element-less-than this upper-bound)
     (sorted-subset-element-less-than (get-delegate this) (get-range this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (sorted-subset-element-greater-than (get-delegate this) (get-range this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (sorted-subset-element-at-most (get-delegate this) (get-range this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (sorted-subset-element-at-least (get-delegate this) (get-range this) lower-bound))

   (define/guard (sorted-subset this element-range)
     (define delegate (get-delegate this))
     (define original-range (get-range this))
     (guard (range-overlaps? original-range element-range) else
       (empty-sorted-set (generic-sorted-set-comparator delegate)))
     (define intersection (range-intersection original-range element-range))
     (constructor:persistent-sorted-subset delegate intersection))

   (define (sorted-set-reverse this)
     (make-reversed-immutable-sorted-set this))]

  #:methods gen:immutable-sorted-set

  [(define (get-tree this)
     (persistent-sorted-set-tree (persistent-sorted-subset-delegate this)))

   (define (get-range this)
     (persistent-sorted-subset-element-range this))

   (define (sorted-set-add this element)
     (define copy (persistent-red-black-subtree-copy (get-tree this) (get-range this)))
     (constructor:persistent-sorted-set (persistent-red-black-tree-insert copy element)))

   (define (sorted-set-remove this element)
     (define copy (persistent-red-black-subtree-copy (get-tree this) (get-range this)))
     (constructor:persistent-sorted-set (persistent-red-black-tree-remove copy element)))])


(define (make-persistent-sorted-set elements #:comparator comparator)
  (for/fold ([tree (empty-persistent-red-black-tree comparator)]
             #:result (constructor:persistent-sorted-set tree))
            ([e elements])
    (persistent-red-black-tree-insert tree e)))


(define (make-persistent-sorted-set-from-sorted sorted-set)
  (make-persistent-sorted-set sorted-set #:comparator (sorted-set-comparator sorted-set)))
