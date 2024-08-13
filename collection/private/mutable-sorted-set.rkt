#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-mutable-sorted-set (->* (#:comparator comparator?) ((sequence/c any/c)) mutable-sorted-set?)]
  [make-mutable-sorted-set-from-sorted (-> sorted-set? mutable-sorted-set?)]))


(module+ private-for-rebellion-only
  (provide
   (contract-out
    [make-empty-mutable-sorted-set (-> comparator? mutable-sorted-set?)])))


(require racket/generic
         racket/sequence
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/private/mutable-red-black-tree
         rebellion/collection/private/reversed-sorted-set
         rebellion/collection/private/sorted-set-interface
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only)
         rebellion/collection/private/sorted-subset
         guard
         rebellion/private/sequence-empty
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(struct regular-mutable-sorted-set abstract-mutable-sorted-set (tree)

  #:constructor-name constructor:regular-mutable-sorted-set

  #:methods gen:sorted-set

  [(define (in-sorted-set this #:descending? [descending? #false])
     (in-mutable-rb-tree-keys (regular-mutable-sorted-set-tree this) #:descending? descending?))

   (define (sorted-set-size this)
     (mutable-rb-tree-size (regular-mutable-sorted-set-tree this)))

   (define (sorted-set-comparator this)
     (mutable-rb-tree-key-comparator (regular-mutable-sorted-set-tree this)))

   (define (sorted-set-contains? this value)
     (mutable-rb-tree-contains-key? (regular-mutable-sorted-set-tree this) value))

   (define (sorted-set-least-element this)
     (mutable-rb-tree-least-key (regular-mutable-sorted-set-tree this)))

   (define (sorted-set-greatest-element this)
     (mutable-rb-tree-greatest-key (regular-mutable-sorted-set-tree this)))

   (define (sorted-set-element-less-than this upper-bound)
     (mutable-rb-tree-key-less-than (regular-mutable-sorted-set-tree this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (mutable-rb-tree-key-greater-than (regular-mutable-sorted-set-tree this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (mutable-rb-tree-key-at-most (regular-mutable-sorted-set-tree this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (mutable-rb-tree-key-at-least (regular-mutable-sorted-set-tree this) lower-bound))

   (define (sorted-subset this element-range)
     (constructor:regular-mutable-sorted-subset this element-range))

   (define (sorted-set-reverse this)
     (make-reversed-mutable-sorted-set this))]

  #:methods gen:mutable-sorted-set

  [(define (sorted-set-add! this element)
     (mutable-rb-tree-put! (regular-mutable-sorted-set-tree this) element #false))

   (define (sorted-set-remove! this element)
     (mutable-rb-tree-remove! (regular-mutable-sorted-set-tree this) element))

   (define (sorted-set-clear! this)
     (mutable-rb-tree-clear! (regular-mutable-sorted-set-tree this)))])


(struct regular-mutable-sorted-subset abstract-mutable-sorted-set (delegate-set element-range)

  #:constructor-name constructor:regular-mutable-sorted-subset

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (regular-mutable-sorted-subset-delegate-set this))

   (define (get-tree this)
     (regular-mutable-sorted-set-tree (get-delegate this)))

   (define (get-range this)
     (regular-mutable-sorted-subset-element-range this))

   (define/generic generic-sorted-set-comparator sorted-set-comparator)

   (define (in-sorted-set this #:descending? [descending? #false])
     (in-mutable-rb-subtree-keys (get-tree this) (get-range this) #:descending? descending?))

   (define (sorted-set-size this)
     (mutable-rb-subtree-size (get-tree this) (get-range this)))

   (define (sorted-set-comparator this)
     (generic-sorted-set-comparator (get-delegate this)))

   (define (sorted-set-contains? this value)
     (sorted-subset-contains? (get-delegate this) (get-range this) value))

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
     (guard (range-overlaps? original-range element-range) #:else
       (make-empty-mutable-sorted-set (generic-sorted-set-comparator delegate)))
     (define intersection (range-intersection original-range element-range))
     (constructor:regular-mutable-sorted-subset delegate intersection))

   (define (sorted-set-reverse this)
     (make-reversed-mutable-sorted-set this))]

  #:methods gen:mutable-sorted-set

  [(define (get-tree this)
     (regular-mutable-sorted-set-tree (regular-mutable-sorted-subset-delegate-set this)))

   (define (get-range this)
     (regular-mutable-sorted-subset-element-range this))

   (define (sorted-set-add! this element)
     (mutable-rb-tree-put! (get-tree this) element #false))

   (define (sorted-set-remove! this element)
     (when (range-contains? (get-range this) element)
       (mutable-rb-tree-remove! (get-tree this) element)))

   (define (sorted-set-clear! this)
     (mutable-rb-subtree-clear! (get-tree this) (get-range this)))])


(struct empty-mutable-sorted-set abstract-mutable-sorted-set (comparator)

  #:constructor-name make-empty-mutable-sorted-set

  #:methods gen:sorted-set

  [(define (in-sorted-set this #:descending? [descending? #false])
     (in-list '()))

   (define (sorted-set-empty? this)
     #true)

   (define (sorted-set-size this)
     0)

   (define (sorted-set-comparator this)
     (empty-mutable-sorted-set-comparator this))

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
     (make-empty-mutable-sorted-set (comparator-reverse (empty-mutable-sorted-set-comparator this))))]

  #:methods gen:mutable-sorted-set

  [(define (sorted-set-add! this element)
     (raise-arguments-error
      (name sorted-set-add!)
      "assertion error: insertion into mutable empty subset shouldn't be possible"))

   (define (sorted-set-add-all! this elements)
     (raise-arguments-error
      (name sorted-set-add!)
      "assertion error: insertion into mutable empty subset shouldn't be possible"))

   (define (sorted-set-remove! this element)
     (void))

   (define (sorted-set-remove-all! this elements)
     (void))])


(define (make-mutable-sorted-set [elements '()] #:comparator comparator)
  (define set (constructor:regular-mutable-sorted-set (make-mutable-rb-tree comparator)))
  (sorted-set-add-all! set elements)
  set)


(define (make-mutable-sorted-set-from-sorted sorted-set)
  (make-mutable-sorted-set sorted-set #:comparator (sorted-set-comparator sorted-set)))
