#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-reversed-unmodifiable-sorted-set (-> sorted-set? sorted-set?)]
  [make-reversed-immutable-sorted-set (-> immutable-sorted-set? immutable-sorted-set?)]
  [make-reversed-mutable-sorted-set (-> mutable-sorted-set? mutable-sorted-set?)]))


(require racket/generic
         racket/sequence
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/private/persistent-red-black-tree
         rebellion/collection/private/sorted-set-interface
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only)
         rebellion/private/guarded-block
         rebellion/private/sequence-empty)


;@----------------------------------------------------------------------------------------------------


(struct reversed-unmodifiable-sorted-set abstract-sorted-set (delegate-set)

  #:constructor-name make-reversed-unmodifiable-sorted-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (reversed-unmodifiable-sorted-set-delegate-set this))

   (define (construct delegate-set)
     (make-reversed-unmodifiable-sorted-set delegate-set))

   (define/generic generic-in-sorted-set in-sorted-set)
   (define/generic generic-sorted-set-empty? sorted-set-empty?)
   (define/generic generic-sorted-set-size sorted-set-size)
   (define/generic generic-sorted-set-comparator sorted-set-comparator)
   (define/generic generic-sorted-set-contains? sorted-set-contains?)
   (define/generic generic-sorted-set-contains-any? sorted-set-contains-any?)
   (define/generic generic-sorted-set-contains-all? sorted-set-contains-all?)
   (define/generic generic-sorted-set-contains-none? sorted-set-contains-none?)
   (define/generic generic-sorted-set-least-element sorted-set-least-element)
   (define/generic generic-sorted-set-greatest-element sorted-set-greatest-element)
   (define/generic generic-sorted-set-element-less-than sorted-set-element-less-than)
   (define/generic generic-sorted-set-element-greater-than sorted-set-element-greater-than)
   (define/generic generic-sorted-set-element-at-least sorted-set-element-at-least)
   (define/generic generic-sorted-set-element-at-most sorted-set-element-at-most)
   (define/generic generic-sorted-subset sorted-subset)

   (define (in-sorted-set this #:descending? [descending? #false])
     (generic-in-sorted-set (get-delegate this) #:descending? (not descending?)))

   (define (sorted-set-empty? this)
     (generic-sorted-set-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (generic-sorted-set-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (comparator-reverse (generic-sorted-set-comparator (get-delegate this))))

   (define (sorted-set-contains? this value)
     (generic-sorted-set-contains? (get-delegate this) value))

   (define (sorted-set-contains-any? this values)
     (generic-sorted-set-contains-any? (get-delegate this) values))

   (define (sorted-set-contains-all? this values)
     (generic-sorted-set-contains-all? (get-delegate this) values))

   (define (sorted-set-contains-none? this values)
     (generic-sorted-set-contains-none? (get-delegate this) values))

   (define (sorted-set-least-element this)
     (generic-sorted-set-greatest-element (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (generic-sorted-set-least-element (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (generic-sorted-set-element-greater-than (get-delegate this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (generic-sorted-set-element-less-than (get-delegate this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (generic-sorted-set-element-at-least (get-delegate this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (generic-sorted-set-element-at-most (get-delegate this) lower-bound))

   (define (sorted-subset this element-range)
     (construct (generic-sorted-subset (get-delegate this) (range-reverse element-range))))

   (define (sorted-set-reverse this)
     (get-delegate this))])


(struct reversed-immutable-sorted-set abstract-immutable-sorted-set (delegate-set)

  #:constructor-name make-reversed-immutable-sorted-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (reversed-immutable-sorted-set-delegate-set this))

   (define (construct delegate-set)
     (make-reversed-immutable-sorted-set delegate-set))

   (define/generic generic-in-sorted-set in-sorted-set)
   (define/generic generic-sorted-set-empty? sorted-set-empty?)
   (define/generic generic-sorted-set-size sorted-set-size)
   (define/generic generic-sorted-set-comparator sorted-set-comparator)
   (define/generic generic-sorted-set-contains? sorted-set-contains?)
   (define/generic generic-sorted-set-contains-any? sorted-set-contains-any?)
   (define/generic generic-sorted-set-contains-all? sorted-set-contains-all?)
   (define/generic generic-sorted-set-contains-none? sorted-set-contains-none?)
   (define/generic generic-sorted-set-least-element sorted-set-least-element)
   (define/generic generic-sorted-set-greatest-element sorted-set-greatest-element)
   (define/generic generic-sorted-set-element-less-than sorted-set-element-less-than)
   (define/generic generic-sorted-set-element-greater-than sorted-set-element-greater-than)
   (define/generic generic-sorted-set-element-at-least sorted-set-element-at-least)
   (define/generic generic-sorted-set-element-at-most sorted-set-element-at-most)
   (define/generic generic-sorted-subset sorted-subset)

   (define (in-sorted-set this #:descending? [descending? #false])
     (generic-in-sorted-set (get-delegate this) #:descending? (not descending?)))

   (define (sorted-set-empty? this)
     (generic-sorted-set-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (generic-sorted-set-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (comparator-reverse (generic-sorted-set-comparator (get-delegate this))))

   (define (sorted-set-contains? this value)
     (generic-sorted-set-contains? (get-delegate this) value))

   (define (sorted-set-contains-any? this values)
     (generic-sorted-set-contains-any? (get-delegate this) values))

   (define (sorted-set-contains-all? this values)
     (generic-sorted-set-contains-all? (get-delegate this) values))

   (define (sorted-set-contains-none? this values)
     (generic-sorted-set-contains-none? (get-delegate this) values))

   (define (sorted-set-least-element this)
     (generic-sorted-set-greatest-element (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (generic-sorted-set-least-element (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (generic-sorted-set-element-greater-than (get-delegate this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (generic-sorted-set-element-less-than (get-delegate this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (generic-sorted-set-element-at-least (get-delegate this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (generic-sorted-set-element-at-most (get-delegate this) lower-bound))

   (define (sorted-subset this element-range)
     (construct (generic-sorted-subset (get-delegate this) (range-reverse element-range))))

   (define (sorted-set-reverse this)
     (get-delegate this))]

  #:methods gen:immutable-sorted-set

  [(define (get-delegate this)
     (reversed-immutable-sorted-set-delegate-set this))

   (define (construct delegate-set)
     (make-reversed-immutable-sorted-set delegate-set))

   (define/generic generic-sorted-set-add sorted-set-add)
   (define/generic generic-sorted-set-add-all sorted-set-add-all)
   (define/generic generic-sorted-set-remove sorted-set-remove)
   (define/generic generic-sorted-set-remove-all sorted-set-remove-all)

   (define (sorted-set-add this element)
     (construct (generic-sorted-set-add (get-delegate this) element)))

   (define (sorted-set-add-all this elements)
     (construct (generic-sorted-set-add-all (get-delegate this) elements)))

   (define (sorted-set-remove this element)
     (construct (generic-sorted-set-remove (get-delegate this) element)))

   (define (sorted-set-remove-all this elements)
     (construct (generic-sorted-set-remove-all (get-delegate this) elements)))])


(struct reversed-mutable-sorted-set abstract-mutable-sorted-set (delegate-set)

  #:constructor-name make-reversed-mutable-sorted-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (reversed-mutable-sorted-set-delegate-set this))

   (define (construct delegate-set)
     (make-reversed-mutable-sorted-set delegate-set))

   (define/generic generic-in-sorted-set in-sorted-set)
   (define/generic generic-sorted-set-empty? sorted-set-empty?)
   (define/generic generic-sorted-set-size sorted-set-size)
   (define/generic generic-sorted-set-comparator sorted-set-comparator)
   (define/generic generic-sorted-set-contains? sorted-set-contains?)
   (define/generic generic-sorted-set-contains-any? sorted-set-contains-any?)
   (define/generic generic-sorted-set-contains-all? sorted-set-contains-all?)
   (define/generic generic-sorted-set-contains-none? sorted-set-contains-none?)
   (define/generic generic-sorted-set-least-element sorted-set-least-element)
   (define/generic generic-sorted-set-greatest-element sorted-set-greatest-element)
   (define/generic generic-sorted-set-element-less-than sorted-set-element-less-than)
   (define/generic generic-sorted-set-element-greater-than sorted-set-element-greater-than)
   (define/generic generic-sorted-set-element-at-least sorted-set-element-at-least)
   (define/generic generic-sorted-set-element-at-most sorted-set-element-at-most)
   (define/generic generic-sorted-subset sorted-subset)

   (define (in-sorted-set this #:descending? [descending? #false])
     (generic-in-sorted-set (get-delegate this) #:descending? (not descending?)))

   (define (sorted-set-empty? this)
     (generic-sorted-set-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (generic-sorted-set-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (comparator-reverse (generic-sorted-set-comparator (get-delegate this))))

   (define (sorted-set-contains? this value)
     (generic-sorted-set-contains? (get-delegate this) value))

   (define (sorted-set-contains-any? this values)
     (generic-sorted-set-contains-any? (get-delegate this) values))

   (define (sorted-set-contains-all? this values)
     (generic-sorted-set-contains-all? (get-delegate this) values))

   (define (sorted-set-contains-none? this values)
     (generic-sorted-set-contains-none? (get-delegate this) values))

   (define (sorted-set-least-element this)
     (generic-sorted-set-greatest-element (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (generic-sorted-set-least-element (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (generic-sorted-set-element-greater-than (get-delegate this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (generic-sorted-set-element-less-than (get-delegate this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (generic-sorted-set-element-at-least (get-delegate this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (generic-sorted-set-element-at-most (get-delegate this) lower-bound))

   (define (sorted-subset this element-range)
     (construct (generic-sorted-subset (get-delegate this) (range-reverse element-range))))

   (define (sorted-set-reverse this)
     (get-delegate this))]

  #:methods gen:mutable-sorted-set

  [(define (get-delegate this)
     (reversed-mutable-sorted-set-delegate-set this))

   (define/generic generic-sorted-set-add! sorted-set-add!)
   (define/generic generic-sorted-set-add-all! sorted-set-add-all!)
   (define/generic generic-sorted-set-remove! sorted-set-remove!)
   (define/generic generic-sorted-set-remove-all! sorted-set-remove-all!)
   (define/generic generic-sorted-set-clear! sorted-set-clear!)

   (define (sorted-set-add! this element)
     (generic-sorted-set-add! (get-delegate this) element))

   (define (sorted-set-add-all this elements)
     (generic-sorted-set-add-all! (get-delegate this) elements))

   (define (sorted-set-remove! this element)
     (generic-sorted-set-remove! (get-delegate this) element))

   (define (sorted-set-remove-all! this elements)
     (generic-sorted-set-remove-all! (get-delegate this) elements))])


(define (range-reverse original-range)
  (range
   (range-upper-bound original-range)
   (range-lower-bound original-range)
   #:comparator (comparator-reverse (range-comparator original-range))))
