#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-unmodifiable-sorted-map-key-set (-> sorted-map? sorted-set?)]
  [make-immutable-sorted-map-key-set (-> immutable-sorted-map? immutable-sorted-set?)]
  [make-mutable-sorted-map-key-set (-> mutable-sorted-map? mutable-sorted-set?)]))


(require racket/generic
         rebellion/collection/private/sorted-map-interface
         rebellion/collection/sorted-set
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only)
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(struct unmodifiable-sorted-map-key-set abstract-sorted-set (delegate-map)

  #:constructor-name make-unmodifiable-sorted-map-key-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (unmodifiable-sorted-map-key-set-delegate-map this))

   (define (construct delegate-map)
     (make-unmodifiable-sorted-map-key-set delegate-map))

   (define (in-sorted-set this #:descending? [descending? #false])
     (in-sorted-map-keys (get-delegate this) #:descending? descending?))

   (define (sorted-set-empty? this)
     (sorted-map-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (sorted-map-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (sorted-map-key-comparator (get-delegate this)))

   (define (sorted-set-contains? this value)
     (sorted-map-contains-key? (get-delegate this) value))

   (define (sorted-set-least-element this)
     (sorted-map-least-key (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (sorted-map-greatest-key (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (sorted-map-key-less-than (get-delegate this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (sorted-map-key-greater-than (get-delegate this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (sorted-map-key-at-most (get-delegate this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (sorted-map-key-at-least (get-delegate this) lower-bound))

   (define (sorted-subset this element-range)
     (construct (sorted-submap (get-delegate this) element-range)))

   (define (sorted-set-reverse this)
     (construct (sorted-map-reverse (get-delegate this))))])


(struct mutable-sorted-map-key-set abstract-sorted-set (delegate-map)

  #:constructor-name make-mutable-sorted-map-key-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (mutable-sorted-map-key-set-delegate-map this))

   (define (construct delegate-map)
     (make-mutable-sorted-map-key-set delegate-map))

   (define (in-sorted-set this #:descending? [descending? #false])
     (in-sorted-map-keys (get-delegate this) #:descending? descending?))

   (define (sorted-set-empty? this)
     (sorted-map-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (sorted-map-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (sorted-map-key-comparator (get-delegate this)))

   (define (sorted-set-contains? this value)
     (sorted-map-contains-key? (get-delegate this) value))

   (define (sorted-set-least-element this)
     (sorted-map-least-key (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (sorted-map-greatest-key (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (sorted-map-key-less-than (get-delegate this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (sorted-map-key-greater-than (get-delegate this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (sorted-map-key-at-most (get-delegate this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (sorted-map-key-at-least (get-delegate this) lower-bound))

   (define (sorted-subset this element-range)
     (construct (sorted-submap (get-delegate this) element-range)))

   (define (sorted-set-reverse this)
     (construct (sorted-map-reverse (get-delegate this))))]

  #:methods gen:mutable-sorted-set

  [(define (get-delegate this)
     (mutable-sorted-map-key-set-delegate-map this))

   (define (sorted-set-add! this element)
     (raise-arguments-error
      (name sorted-set-add)
      "sorted map key sets do not support insertion"
      "key set" this
      "element" element))

   (define (sorted-set-add-all! this elements)
     (raise-arguments-error
      (name sorted-set-add-all)
      "sorted map key sets do not support insertion"
      "key set" this
      "elements" elements))

   (define (sorted-set-remove! this element)
     (sorted-map-remove! (get-delegate this) element))

   (define (sorted-set-remove-all! this elements)
     (sorted-map-remove-all! (get-delegate this) elements))

   (define (sorted-set-clear! this)
     (sorted-map-clear! (get-delegate this)))])


(struct immutable-sorted-map-key-set abstract-sorted-set (delegate-map)

  #:constructor-name make-immutable-sorted-map-key-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (immutable-sorted-map-key-set-delegate-map this))

   (define (construct delegate-map)
     (make-immutable-sorted-map-key-set delegate-map))

   (define (in-sorted-set this #:descending? [descending? #false])
     (in-sorted-map-keys (get-delegate this) #:descending? descending?))

   (define (sorted-set-empty? this)
     (sorted-map-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (sorted-map-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (sorted-map-key-comparator (get-delegate this)))

   (define (sorted-set-contains? this value)
     (sorted-map-contains-key? (get-delegate this) value))

   (define (sorted-set-least-element this)
     (sorted-map-least-key (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (sorted-map-greatest-key (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (sorted-map-key-less-than (get-delegate this) upper-bound))

   (define (sorted-set-element-greater-than this lower-bound)
     (sorted-map-key-greater-than (get-delegate this) lower-bound))

   (define (sorted-set-element-at-most this upper-bound)
     (sorted-map-key-at-most (get-delegate this) upper-bound))
   
   (define (sorted-set-element-at-least this lower-bound)
     (sorted-map-key-at-least (get-delegate this) lower-bound))

   (define (sorted-subset this element-range)
     (construct (sorted-submap (get-delegate this) element-range)))

   (define (sorted-set-reverse this)
     (construct (sorted-map-reverse (get-delegate this))))]

  #:methods gen:immutable-sorted-set

  [(define/generic generic-sorted-set-add sorted-set-add)
   (define/generic generic-sorted-set-add-all sorted-set-add-all)
   (define/generic generic-sorted-set-remove sorted-set-remove)
   (define/generic generic-sorted-set-remove-all sorted-set-remove-all)

   (define (copy this)
     (sorted-set->immutable-sorted-set (immutable-sorted-map-key-set-delegate-map this)))

   (define (sorted-set-add this element)
     (generic-sorted-set-add (copy this) element))

   (define (sorted-set-add-all this elements)
     (generic-sorted-set-add-all (copy this) elements))

   (define (sorted-set-remove this element)
     (generic-sorted-set-remove (copy this) element))

   (define (sorted-set-remove-all this elements)
     (generic-sorted-set-remove-all (copy this) elements))])
