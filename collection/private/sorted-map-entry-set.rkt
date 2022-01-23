#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-unmodifiable-sorted-map-entry-set (-> sorted-map? sorted-set?)]
  [make-immutable-sorted-map-entry-set (-> immutable-sorted-map? immutable-sorted-set?)]
  [make-mutable-sorted-map-entry-set (-> mutable-sorted-map? mutable-sorted-set?)]))


(module+ private-for-rebellion-only
  (provide
   (contract-out
    [make-entry-comparator (-> comparator? (comparator/c entry?))])))


(require racket/generic
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/private/sorted-map-interface
         rebellion/collection/sorted-set
         (submod rebellion/collection/private/sorted-set-interface private-for-rebellion-only)
         rebellion/private/guarded-block
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(struct unmodifiable-sorted-map-entry-set abstract-sorted-set (delegate-map)

  #:constructor-name make-unmodifiable-sorted-map-entry-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (unmodifiable-sorted-map-entry-set-delegate-map this))

   (define (construct delegate-map)
     (make-unmodifiable-sorted-map-entry-set delegate-map))

   (define (in-sorted-set this #:descending? [descending? #false])
     (in-sorted-map (get-delegate this) #:descending? descending?))

   (define (sorted-set-empty? this)
     (sorted-map-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (sorted-map-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (make-entry-comparator (sorted-map-key-comparator (get-delegate this))))

   (define (sorted-set-contains? this value)
     (and (entry? value) (sorted-map-contains-entry? (get-delegate this) value)))

   (define (sorted-set-least-element this)
     (sorted-map-least-entry (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (sorted-map-greatest-entry (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (sorted-map-entry-less-than (get-delegate this) (entry-key upper-bound)))

   (define (sorted-set-element-greater-than this lower-bound)
     (sorted-map-entry-greater-than (get-delegate this) (entry-key lower-bound)))

   (define (sorted-set-element-at-most this upper-bound)
     (sorted-map-entry-at-most (get-delegate this) (entry-key upper-bound)))
   
   (define (sorted-set-element-at-least this lower-bound)
     (sorted-map-entry-at-least (get-delegate this) (entry-key lower-bound)))

   (define (sorted-subset this entry-range)
     (define key<=> (sorted-map-key-comparator (get-delegate this)))
     (define key-range (make-key-range entry-range #:key-comparator key<=>))
     (construct (sorted-submap (get-delegate this) key-range)))

   (define (sorted-set-reverse this)
     (construct (sorted-map-reverse (get-delegate this))))])


(struct mutable-sorted-map-entry-set abstract-sorted-set (delegate-map)

  #:constructor-name make-mutable-sorted-map-entry-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (mutable-sorted-map-entry-set-delegate-map this))

   (define (construct delegate-map)
     (make-mutable-sorted-map-entry-set delegate-map))

   (define (in-sorted-set this #:descending? [descending? #false])
     (in-sorted-map (get-delegate this) #:descending? descending?))

   (define (sorted-set-empty? this)
     (sorted-map-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (sorted-map-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (make-entry-comparator (sorted-map-key-comparator (get-delegate this))))

   (define (sorted-set-contains? this value)
     (and (entry? value) (sorted-map-contains-entry? (get-delegate this) value)))

   (define (sorted-set-least-element this)
     (sorted-map-least-entry (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (sorted-map-greatest-entry (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (sorted-map-entry-less-than (get-delegate this) (entry-key upper-bound)))

   (define (sorted-set-element-greater-than this lower-bound)
     (sorted-map-entry-greater-than (get-delegate this) (entry-key lower-bound)))

   (define (sorted-set-element-at-most this upper-bound)
     (sorted-map-entry-at-most (get-delegate this) (entry-key upper-bound)))
   
   (define (sorted-set-element-at-least this lower-bound)
     (sorted-map-entry-at-least (get-delegate this) (entry-key lower-bound)))

   (define (sorted-subset this entry-range)
     (define key<=> (sorted-map-key-comparator (get-delegate this)))
     (define key-range (make-key-range entry-range #:key-comparator key<=>))
     (construct (sorted-submap (get-delegate this) key-range)))

   (define (sorted-set-reverse this)
     (construct (sorted-map-reverse (get-delegate this))))]

  #:methods gen:mutable-sorted-set

  [(define (get-delegate this)
     (mutable-sorted-map-entry-set-delegate-map this))

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


(struct immutable-sorted-map-entry-set abstract-sorted-set (delegate-map)

  #:constructor-name make-immutable-sorted-map-entry-set

  #:methods gen:sorted-set

  [(define (get-delegate this)
     (immutable-sorted-map-entry-set-delegate-map this))

   (define (construct delegate-map)
     (make-immutable-sorted-map-entry-set delegate-map))

   (define (in-sorted-set this #:descending? [descending? #false])
     (in-sorted-map (get-delegate this) #:descending? descending?))

   (define (sorted-set-empty? this)
     (sorted-map-empty? (get-delegate this)))

   (define (sorted-set-size this)
     (sorted-map-size (get-delegate this)))

   (define (sorted-set-comparator this)
     (make-entry-comparator (sorted-map-key-comparator (get-delegate this))))

   (define (sorted-set-contains? this value)
     (and (entry? value) (sorted-map-contains-entry? (get-delegate this) value)))

   (define (sorted-set-least-element this)
     (sorted-map-least-entry (get-delegate this)))

   (define (sorted-set-greatest-element this)
     (sorted-map-greatest-entry (get-delegate this)))

   (define (sorted-set-element-less-than this upper-bound)
     (sorted-map-entry-less-than (get-delegate this) (entry-key upper-bound)))

   (define (sorted-set-element-greater-than this lower-bound)
     (sorted-map-entry-greater-than (get-delegate this) (entry-key lower-bound)))

   (define (sorted-set-element-at-most this upper-bound)
     (sorted-map-entry-at-most (get-delegate this) (entry-key upper-bound)))
   
   (define (sorted-set-element-at-least this lower-bound)
     (sorted-map-entry-at-least (get-delegate this) (entry-key lower-bound)))

   (define (sorted-subset this entry-range)
     (define key<=> (sorted-map-key-comparator (get-delegate this)))
     (define key-range (make-key-range entry-range #:key-comparator key<=>))
     (construct (sorted-submap (get-delegate this) key-range)))

   (define (sorted-set-reverse this)
     (construct (sorted-map-reverse (get-delegate this))))]

  #:methods gen:immutable-sorted-set

  [(define/generic generic-sorted-set-add sorted-set-add)
   (define/generic generic-sorted-set-add-all sorted-set-add-all)
   (define/generic generic-sorted-set-remove sorted-set-remove)
   (define/generic generic-sorted-set-remove-all sorted-set-remove-all)

   (define (sorted-set-add this element)
     (generic-sorted-set-add (sorted-set->immutable-sorted-set this) element))

   (define (sorted-set-add-all this elements)
     (generic-sorted-set-add-all (sorted-set->immutable-sorted-set this) elements))

   (define (sorted-set-remove this element)
     (generic-sorted-set-remove (sorted-set->immutable-sorted-set this) element))

   (define (sorted-set-remove-all this elements)
     (generic-sorted-set-remove-all (sorted-set->immutable-sorted-set this) elements))])


(define entry-comparator-cache (make-ephemeron-hash))


(define (make-entry-comparator key-comparator)
  (hash-ref! entry-comparator-cache key-comparator (λ () (comparator-map key-comparator entry-key))))


(define (make-key-range entry-range #:key-comparator key-comparator)
  (define lower (range-lower-bound entry-range))
  (define upper (range-upper-bound entry-range))
  (range (make-key-bound lower) (make-key-bound upper) #:comparator key-comparator))


(define/guard (make-key-bound bound)
  (guard (unbounded? bound) then
    unbounded)
  (define key (entry-key (range-bound-endpoint bound)))
  (range-bound key (range-bound-type bound)))
