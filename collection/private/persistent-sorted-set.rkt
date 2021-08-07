#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-empty-sorted-set (-> comparator? immutable-sorted-set?)]
  [make-persistent-sorted-set
   (-> (sequence/c any/c) #:comparator comparator? immutable-sorted-set?)]
  [make-persistent-sorted-set-from-sorted (-> sorted-set? immutable-sorted-set?)]))


(require racket/sequence
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/private/persistent-red-black-tree
         rebellion/collection/private/sorted-set-interface
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only)
         rebellion/private/guarded-block
         rebellion/private/sequence-empty)


;@----------------------------------------------------------------------------------------------------


(struct empty-sorted-set abstract-immutable-sorted-set (comparator)

  #:constructor-name make-empty-sorted-set

  #:methods gen:sorted-set

  [(define (in-sorted-set this)
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
     (make-empty-sorted-set (comparator-reverse (empty-sorted-set-comparator this))))]

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

  [(define (in-sorted-set this)
     (in-persistent-red-black-tree (persistent-sorted-set-tree this)))

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
     (define tree (persistent-sorted-set-tree this))
     (constructor:reversed-persistent-sorted-set tree))]

  #:methods gen:immutable-sorted-set

  [(define (sorted-set-add this element)
     (define tree (persistent-sorted-set-tree this))
     (constructor:persistent-sorted-set (persistent-red-black-tree-insert tree element)))

   (define (sorted-set-remove this element)
     (define tree (persistent-sorted-set-tree this))
     (constructor:persistent-sorted-set (persistent-red-black-tree-remove tree element)))])


(struct persistent-sorted-subset abstract-immutable-sorted-set (tree element-range)

  #:constructor-name constructor:persistent-sorted-subset

  #:methods gen:sorted-set

  [(define (in-sorted-set this)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (in-persistent-red-black-subtree tree))

   (define (sorted-set-size this)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-size tree range))

   (define (sorted-set-comparator this)
     (persistent-red-black-tree-comparator (persistent-sorted-subset-tree this)))

   (define (sorted-set-contains? this value)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-contains? tree range value))

   (define/guard (sorted-set-least-element this)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-least-element tree range))

   (define/guard (sorted-set-greatest-element this)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-greatest-element tree range))

   (define/guard (sorted-set-element-less-than this upper-bound)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-element-less-than tree range upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-element-greater-than tree range lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-element-at-most tree range upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-element-at-least tree range lower-bound))

   (define/guard (sorted-subset this element-range)
     (define tree (persistent-sorted-subset-tree this))
     (define original-range (persistent-sorted-subset-element-range this))
     (guard (range-overlaps? original-range element-range) else
       (make-empty-sorted-set (persistent-red-black-tree-comparator tree)))
     (define intersection (range-intersection original-range element-range))
     (constructor:persistent-sorted-subset tree intersection))

   (define (sorted-set-reverse this)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (constructor:reversed-persistent-sorted-subset tree range))]

  #:methods gen:immutable-sorted-set

  [(define (sorted-set-add this element)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (define copy (persistent-red-black-subtree-copy tree range))
     (constructor:persistent-sorted-set (persistent-red-black-tree-insert copy element)))

   (define (sorted-set-remove this element)
     (define tree (persistent-sorted-subset-tree this))
     (define range (persistent-sorted-subset-element-range this))
     (define copy (persistent-red-black-subtree-copy tree range))
     (constructor:persistent-sorted-set (persistent-red-black-tree-remove copy element)))])


(struct reversed-persistent-sorted-set abstract-immutable-sorted-set (tree)

  #:constructor-name constructor:reversed-persistent-sorted-set

  #:methods gen:sorted-set

  [(define (in-sorted-set this)
     (in-persistent-red-black-tree (reversed-persistent-sorted-set-tree this) #:descending? #true))

   (define (sorted-set-size this)
     (persistent-red-black-tree-size (reversed-persistent-sorted-set-tree this)))

   (define (sorted-set-comparator this)
     (define tree (reversed-persistent-sorted-set-tree this))
     (comparator-reverse (persistent-red-black-tree-comparator tree)))

   (define (sorted-set-contains? this value)
     (persistent-red-black-tree-contains? (persistent-sorted-subset-tree this) value))

   (define (sorted-set-least-element this)
     (persistent-red-black-tree-greatest-element (persistent-sorted-subset-tree this)))

   (define (sorted-set-greatest-element this)
     (persistent-red-black-tree-least-element (persistent-sorted-subset-tree this)))

   (define (sorted-set-element-less-than this upper-bound)
     (persistent-red-black-tree-element-at-least (persistent-sorted-subset-tree this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (persistent-red-black-tree-element-at-most (persistent-sorted-subset-tree this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (define tree (persistent-sorted-subset-tree this))
     (persistent-red-black-tree-element-greater-than tree upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (define tree (persistent-sorted-subset-tree this))
     (persistent-red-black-tree-element-less-than tree lower-bound))

   (define (sorted-subset this element-range)
     (define tree (persistent-sorted-subset-tree this))
     (constructor:reversed-persistent-sorted-subset tree element-range))

   (define (sorted-set-reverse this)
     (constructor:persistent-sorted-set (reversed-persistent-sorted-set-tree this)))]

  #:methods gen:immutable-sorted-set

  [(define (sorted-set-add this element)
     (define tree (reversed-persistent-sorted-set-tree this))
     (constructor:reversed-persistent-sorted-set (persistent-red-black-tree-insert tree element)))

   (define (sorted-set-remove this element)
     (define tree (reversed-persistent-sorted-set-tree this))
     (constructor:reversed-persistent-sorted-set (persistent-red-black-tree-remove tree element)))])


(struct reversed-persistent-sorted-subset abstract-immutable-sorted-set (tree element-range)

  #:constructor-name constructor:reversed-persistent-sorted-subset

  #:methods gen:sorted-set

  [(define (in-sorted-set this)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (in-persistent-red-black-subtree tree range #:descending? #true))

   (define (sorted-set-size this)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-size tree range))

   (define (sorted-set-comparator this)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (comparator-reverse (persistent-red-black-tree-comparator tree)))

   (define (sorted-set-contains? this value)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-contains? tree range value))

   (define (sorted-set-least-element this)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-greatest-element tree range))

   (define (sorted-set-greatest-element this)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-least-element tree range))

   (define (sorted-set-element-less-than this upper-bound)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-element-at-least tree range upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-element-at-most tree range lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-element-greater-than tree range upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (persistent-red-black-subtree-element-less-than tree range lower-bound))

   (define/guard (sorted-subset this element-range)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define original-range (reversed-persistent-sorted-subset-element-range this))
     (guard (range-overlaps? original-range element-range) else
       (make-empty-sorted-set
        (comparator-reverse (persistent-red-black-tree-comparator tree))))
     (define intersection (range-intersection original-range element-range))
     (constructor:reversed-persistent-sorted-subset tree intersection))

   (define (sorted-set-reverse this)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (constructor:persistent-sorted-subset tree range))]

  #:methods gen:immutable-sorted-set

  [(define (sorted-set-add this element)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (define copy (persistent-red-black-subtree-copy tree range))
     (constructor:reversed-persistent-sorted-set (persistent-red-black-tree-insert copy element)))

   (define (sorted-set-remove this element)
     (define tree (reversed-persistent-sorted-subset-tree this))
     (define range (reversed-persistent-sorted-subset-element-range this))
     (define copy (persistent-red-black-subtree-copy tree range))
     (constructor:reversed-persistent-sorted-set (persistent-red-black-tree-remove copy element)))])


(define (make-persistent-sorted-set elements #:comparator comparator)
  (for/fold ([tree (empty-persistent-red-black-tree comparator)]
             #:result (constructor:persistent-sorted-set tree))
            ([e elements])
    (persistent-red-black-tree-insert tree e)))


(define (make-persistent-sorted-set-from-sorted sorted-set)
  (make-persistent-sorted-set sorted-set #:comparator (sorted-set-comparator sorted-set)))
