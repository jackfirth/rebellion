#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-reversed-unmodifiable-sorted-map (-> sorted-map? sorted-map?)]
  [make-reversed-immutable-sorted-map (-> immutable-sorted-map? immutable-sorted-map?)]
  [make-reversed-mutable-sorted-map (-> mutable-sorted-map? mutable-sorted-map?)]))


(require racket/generic
         racket/match
         rebellion/base/comparator
         rebellion/base/range
         rebellion/base/result
         rebellion/collection/private/sorted-map-interface
         (submod rebellion/collection/private/sorted-map-interface private-for-rebellion-only)
         rebellion/collection/private/sorted-set-interface
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(struct reversed-unmodifiable-sorted-map abstract-sorted-map (delegate-map)

  #:constructor-name make-reversed-unmodifiable-sorted-map

  #:methods gen:sorted-map

  [(define (get-delegate this)
     (reversed-unmodifiable-sorted-map-delegate-map this))

   (define/generic generic-in-sorted-map in-sorted-map)
   (define/generic generic-in-sorted-map-keys in-sorted-map-keys)
   (define/generic generic-in-sorted-map-values in-sorted-map-values)
   (define/generic generic-sorted-map-empty? sorted-map-empty?)
   (define/generic generic-sorted-map-size sorted-map-size)
   (define/generic generic-sorted-map-key-comparator sorted-map-key-comparator)
   (define/generic generic-sorted-map-contains-key? sorted-map-contains-key?)
   (define/generic generic-sorted-map-contains-value? sorted-map-contains-value?)
   (define/generic generic-sorted-map-contains-entry? sorted-map-contains-entry?)
   (define/generic generic-sorted-map-get sorted-map-get)
   (define/generic generic-sorted-map-get-entry sorted-map-get-entry)
   (define/generic generic-sorted-map-get-option sorted-map-get-option)
   (define/generic generic-sorted-map-least-key sorted-map-least-key)
   (define/generic generic-sorted-map-greatest-key sorted-map-greatest-key)
   (define/generic generic-sorted-map-key-less-than sorted-map-key-less-than)
   (define/generic generic-sorted-map-key-at-most sorted-map-key-at-most)
   (define/generic generic-sorted-map-key-greater-than sorted-map-key-greater-than)
   (define/generic generic-sorted-map-key-at-least sorted-map-key-at-least)
   (define/generic generic-sorted-map-least-entry sorted-map-least-entry)
   (define/generic generic-sorted-map-greatest-entry sorted-map-greatest-entry)
   (define/generic generic-sorted-map-entry-less-than sorted-map-entry-less-than)
   (define/generic generic-sorted-map-entry-at-most sorted-map-entry-at-most)
   (define/generic generic-sorted-map-entry-greater-than sorted-map-entry-greater-than)
   (define/generic generic-sorted-map-entry-at-least sorted-map-entry-at-least)
   (define/generic generic-sorted-map-entries sorted-map-entries)
   (define/generic generic-sorted-map-keys sorted-map-keys)
   (define/generic generic-sorted-submap sorted-submap)
   
   (define (in-sorted-map this #:descending? [descending? #false])
     (generic-in-sorted-map (get-delegate this) #:descending? (not descending?)))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (generic-in-sorted-map-keys (get-delegate this) #:descending? (not descending?)))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (generic-in-sorted-map-values (get-delegate this) #:descending? (not descending?)))
  
   (define (sorted-map-empty? this)
     (generic-sorted-map-empty? (get-delegate this)))

   (define (sorted-map-size this)
     (generic-sorted-map-size (get-delegate this)))

   (define (sorted-map-key-comparator this)
     (comparator-reverse (generic-sorted-map-key-comparator (get-delegate this))))

   (define (sorted-map-contains-key? this key)
     (generic-sorted-map-contains-key? (get-delegate this) key))

   (define (sorted-map-contains-value? this value)
     (generic-sorted-map-contains-value? (get-delegate this) value))

   (define (sorted-map-contains-entry? this entry)
     (generic-sorted-map-contains-entry? (get-delegate this) entry))

   (define (sorted-map-get
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (generic-sorted-map-get (get-delegate this) key failure-result))

   (define (sorted-map-get-option this key)
     (generic-sorted-map-get-option (get-delegate this) key))

   (define (sorted-map-get-entry
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (generic-sorted-map-get-entry (get-delegate this) key failure-result))

   (define (sorted-map-least-key this)
     (generic-sorted-map-greatest-key (get-delegate this)))

   (define (sorted-map-greatest-key this)
     (generic-sorted-map-least-key (get-delegate this)))

   (define (sorted-map-key-less-than this upper-bound)
     (generic-sorted-map-key-greater-than (get-delegate this) upper-bound))

   (define (sorted-map-key-at-most this upper-bound)
     (generic-sorted-map-key-at-least (get-delegate this) upper-bound))

   (define (sorted-map-key-greater-than this lower-bound)
     (generic-sorted-map-key-less-than (get-delegate this) lower-bound))

   (define (sorted-map-key-at-least this lower-bound)
     (generic-sorted-map-key-at-most (get-delegate this) lower-bound))

   (define (sorted-map-least-entry this)
     (generic-sorted-map-greatest-entry (get-delegate this)))

   (define (sorted-map-greatest-entry this)
     (generic-sorted-map-least-entry (get-delegate this)))

   (define (sorted-map-entry-less-than this upper-bound)
     (generic-sorted-map-entry-greater-than (get-delegate this) upper-bound))

   (define (sorted-map-entry-at-most this upper-bound)
     (generic-sorted-map-entry-at-least (get-delegate this) upper-bound))

   (define (sorted-map-entry-greater-than this lower-bound)
     (generic-sorted-map-entry-less-than (get-delegate this) lower-bound))

   (define (sorted-map-entry-at-least this lower-bound)
     (generic-sorted-map-entry-at-most (get-delegate this) lower-bound))

   (define (sorted-map-keys this)
     (sorted-set-reverse (generic-sorted-map-keys (get-delegate this))))

   (define (sorted-map-entries this)
     (sorted-set-reverse (generic-sorted-map-entries (get-delegate this))))

   (define (sorted-submap this key-range)
     (make-reversed-unmodifiable-sorted-map
      (generic-sorted-submap (get-delegate this) (range-reverse key-range))))

   (define (sorted-map-reverse this)
     (get-delegate this))])


(struct reversed-immutable-sorted-map abstract-immutable-sorted-map (delegate-map)

  #:constructor-name make-reversed-immutable-sorted-map

  #:methods gen:sorted-map

  [(define (get-delegate this)
     (reversed-immutable-sorted-map-delegate-map this))

   (define/generic generic-in-sorted-map in-sorted-map)
   (define/generic generic-in-sorted-map-keys in-sorted-map-keys)
   (define/generic generic-in-sorted-map-values in-sorted-map-values)
   (define/generic generic-sorted-map-empty? sorted-map-empty?)
   (define/generic generic-sorted-map-size sorted-map-size)
   (define/generic generic-sorted-map-key-comparator sorted-map-key-comparator)
   (define/generic generic-sorted-map-contains-key? sorted-map-contains-key?)
   (define/generic generic-sorted-map-contains-value? sorted-map-contains-value?)
   (define/generic generic-sorted-map-contains-entry? sorted-map-contains-entry?)
   (define/generic generic-sorted-map-get sorted-map-get)
   (define/generic generic-sorted-map-get-entry sorted-map-get-entry)
   (define/generic generic-sorted-map-get-option sorted-map-get-option)
   (define/generic generic-sorted-map-least-key sorted-map-least-key)
   (define/generic generic-sorted-map-greatest-key sorted-map-greatest-key)
   (define/generic generic-sorted-map-key-less-than sorted-map-key-less-than)
   (define/generic generic-sorted-map-key-at-most sorted-map-key-at-most)
   (define/generic generic-sorted-map-key-greater-than sorted-map-key-greater-than)
   (define/generic generic-sorted-map-key-at-least sorted-map-key-at-least)
   (define/generic generic-sorted-map-least-entry sorted-map-least-entry)
   (define/generic generic-sorted-map-greatest-entry sorted-map-greatest-entry)
   (define/generic generic-sorted-map-entry-less-than sorted-map-entry-less-than)
   (define/generic generic-sorted-map-entry-at-most sorted-map-entry-at-most)
   (define/generic generic-sorted-map-entry-greater-than sorted-map-entry-greater-than)
   (define/generic generic-sorted-map-entry-at-least sorted-map-entry-at-least)
   (define/generic generic-sorted-map-entries sorted-map-entries)
   (define/generic generic-sorted-map-keys sorted-map-keys)
   (define/generic generic-sorted-submap sorted-submap)
   
   (define (in-sorted-map this #:descending? [descending? #false])
     (generic-in-sorted-map (get-delegate this) #:descending? (not descending?)))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (generic-in-sorted-map-keys (get-delegate this) #:descending? (not descending?)))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (generic-in-sorted-map-values (get-delegate this) #:descending? (not descending?)))
  
   (define (sorted-map-empty? this)
     (generic-sorted-map-empty? (get-delegate this)))

   (define (sorted-map-size this)
     (generic-sorted-map-size (get-delegate this)))

   (define (sorted-map-key-comparator this)
     (comparator-reverse (generic-sorted-map-key-comparator (get-delegate this))))

   (define (sorted-map-contains-key? this key)
     (generic-sorted-map-contains-key? (get-delegate this) key))

   (define (sorted-map-contains-value? this value)
     (generic-sorted-map-contains-value? (get-delegate this) value))

   (define (sorted-map-contains-entry? this entry)
     (generic-sorted-map-contains-entry? (get-delegate this) entry))

   (define (sorted-map-get
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (generic-sorted-map-get (get-delegate this) key failure-result))

   (define (sorted-map-get-option this key)
     (generic-sorted-map-get-option (get-delegate this) key))

   (define (sorted-map-get-entry
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (generic-sorted-map-get-entry (get-delegate this) key failure-result))

   (define (sorted-map-least-key this)
     (generic-sorted-map-greatest-key (get-delegate this)))

   (define (sorted-map-greatest-key this)
     (generic-sorted-map-least-key (get-delegate this)))

   (define (sorted-map-key-less-than this upper-bound)
     (generic-sorted-map-key-greater-than (get-delegate this) upper-bound))

   (define (sorted-map-key-at-most this upper-bound)
     (generic-sorted-map-key-at-least (get-delegate this) upper-bound))

   (define (sorted-map-key-greater-than this lower-bound)
     (generic-sorted-map-key-less-than (get-delegate this) lower-bound))

   (define (sorted-map-key-at-least this lower-bound)
     (generic-sorted-map-key-at-most (get-delegate this) lower-bound))

   (define (sorted-map-least-entry this)
     (generic-sorted-map-greatest-entry (get-delegate this)))

   (define (sorted-map-greatest-entry this)
     (generic-sorted-map-least-entry (get-delegate this)))

   (define (sorted-map-entry-less-than this upper-bound)
     (generic-sorted-map-entry-greater-than (get-delegate this) upper-bound))

   (define (sorted-map-entry-at-most this upper-bound)
     (generic-sorted-map-entry-at-least (get-delegate this) upper-bound))

   (define (sorted-map-entry-greater-than this lower-bound)
     (generic-sorted-map-entry-less-than (get-delegate this) lower-bound))

   (define (sorted-map-entry-at-least this lower-bound)
     (generic-sorted-map-entry-at-most (get-delegate this) lower-bound))

   (define (sorted-map-keys this)
     (sorted-set-reverse (generic-sorted-map-keys (get-delegate this))))

   (define (sorted-map-entries this)
     (sorted-set-reverse (generic-sorted-map-entries (get-delegate this))))

   (define (sorted-submap this key-range)
     (make-reversed-immutable-sorted-map
      (generic-sorted-submap (get-delegate this) (range-reverse key-range))))

   (define (sorted-map-reverse this)
     (get-delegate this))]

  #:methods gen:immutable-sorted-map

  [(define (get-delegate this)
     (reversed-immutable-sorted-map-delegate-map this))

   (define (construct delegate-map)
     (make-reversed-immutable-sorted-map delegate-map))

   (define/generic generic-sorted-map-put sorted-map-put)
   (define/generic generic-sorted-map-put-all sorted-map-put-all)
   (define/generic generic-sorted-map-put-if-absent sorted-map-put-if-absent)
   (define/generic generic-sorted-map-remove sorted-map-remove)
   (define/generic generic-sorted-map-remove-all sorted-map-remove-all)

   (define (sorted-map-put this key value)
     (construct (generic-sorted-map-put (get-delegate this) key value)))

   (define (sorted-map-put-all this entries)
     (construct (generic-sorted-map-put-all (get-delegate this) entries)))

   (define (sorted-map-put-if-absent this key value)
     (match (generic-sorted-map-put-if-absent (get-delegate this) key value)
       [(success new-delegate) (success (construct new-delegate))]
       [fail fail]))

   (define (sorted-map-remove this key)
     (construct (generic-sorted-map-remove (get-delegate this) key)))

   (define (sorted-map-remove-all this keys)
     (construct (generic-sorted-map-remove-all (get-delegate this) keys)))])


(struct reversed-mutable-sorted-map abstract-mutable-sorted-map (delegate-map)

  #:constructor-name make-reversed-mutable-sorted-map

  #:methods gen:sorted-map

  [(define (get-delegate this)
     (reversed-mutable-sorted-map-delegate-map this))

   (define/generic generic-in-sorted-map in-sorted-map)
   (define/generic generic-in-sorted-map-keys in-sorted-map-keys)
   (define/generic generic-in-sorted-map-values in-sorted-map-values)
   (define/generic generic-sorted-map-empty? sorted-map-empty?)
   (define/generic generic-sorted-map-size sorted-map-size)
   (define/generic generic-sorted-map-key-comparator sorted-map-key-comparator)
   (define/generic generic-sorted-map-contains-key? sorted-map-contains-key?)
   (define/generic generic-sorted-map-contains-value? sorted-map-contains-value?)
   (define/generic generic-sorted-map-contains-entry? sorted-map-contains-entry?)
   (define/generic generic-sorted-map-get sorted-map-get)
   (define/generic generic-sorted-map-get-entry sorted-map-get-entry)
   (define/generic generic-sorted-map-get-option sorted-map-get-option)
   (define/generic generic-sorted-map-least-key sorted-map-least-key)
   (define/generic generic-sorted-map-greatest-key sorted-map-greatest-key)
   (define/generic generic-sorted-map-key-less-than sorted-map-key-less-than)
   (define/generic generic-sorted-map-key-at-most sorted-map-key-at-most)
   (define/generic generic-sorted-map-key-greater-than sorted-map-key-greater-than)
   (define/generic generic-sorted-map-key-at-least sorted-map-key-at-least)
   (define/generic generic-sorted-map-least-entry sorted-map-least-entry)
   (define/generic generic-sorted-map-greatest-entry sorted-map-greatest-entry)
   (define/generic generic-sorted-map-entry-less-than sorted-map-entry-less-than)
   (define/generic generic-sorted-map-entry-at-most sorted-map-entry-at-most)
   (define/generic generic-sorted-map-entry-greater-than sorted-map-entry-greater-than)
   (define/generic generic-sorted-map-entry-at-least sorted-map-entry-at-least)
   (define/generic generic-sorted-map-entries sorted-map-entries)
   (define/generic generic-sorted-map-keys sorted-map-keys)
   (define/generic generic-sorted-submap sorted-submap)
   
   (define (in-sorted-map this #:descending? [descending? #false])
     (generic-in-sorted-map (get-delegate this) #:descending? (not descending?)))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (generic-in-sorted-map-keys (get-delegate this) #:descending? (not descending?)))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (generic-in-sorted-map-values (get-delegate this) #:descending? (not descending?)))
  
   (define (sorted-map-empty? this)
     (generic-sorted-map-empty? (get-delegate this)))

   (define (sorted-map-size this)
     (generic-sorted-map-size (get-delegate this)))

   (define (sorted-map-key-comparator this)
     (comparator-reverse (generic-sorted-map-key-comparator (get-delegate this))))

   (define (sorted-map-contains-key? this key)
     (generic-sorted-map-contains-key? (get-delegate this) key))

   (define (sorted-map-contains-value? this value)
     (generic-sorted-map-contains-value? (get-delegate this) value))

   (define (sorted-map-contains-entry? this entry)
     (generic-sorted-map-contains-entry? (get-delegate this) entry))

   (define (sorted-map-get
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (generic-sorted-map-get (get-delegate this) key failure-result))

   (define (sorted-map-get-option this key)
     (generic-sorted-map-get-option (get-delegate this) key))

   (define (sorted-map-get-entry
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (generic-sorted-map-get-entry (get-delegate this) key failure-result))

   (define (sorted-map-least-key this)
     (generic-sorted-map-greatest-key (get-delegate this)))

   (define (sorted-map-greatest-key this)
     (generic-sorted-map-least-key (get-delegate this)))

   (define (sorted-map-key-less-than this upper-bound)
     (generic-sorted-map-key-greater-than (get-delegate this) upper-bound))

   (define (sorted-map-key-at-most this upper-bound)
     (generic-sorted-map-key-at-least (get-delegate this) upper-bound))

   (define (sorted-map-key-greater-than this lower-bound)
     (generic-sorted-map-key-less-than (get-delegate this) lower-bound))

   (define (sorted-map-key-at-least this lower-bound)
     (generic-sorted-map-key-at-most (get-delegate this) lower-bound))

   (define (sorted-map-least-entry this)
     (generic-sorted-map-greatest-entry (get-delegate this)))

   (define (sorted-map-greatest-entry this)
     (generic-sorted-map-least-entry (get-delegate this)))

   (define (sorted-map-entry-less-than this upper-bound)
     (generic-sorted-map-entry-greater-than (get-delegate this) upper-bound))

   (define (sorted-map-entry-at-most this upper-bound)
     (generic-sorted-map-entry-at-least (get-delegate this) upper-bound))

   (define (sorted-map-entry-greater-than this lower-bound)
     (generic-sorted-map-entry-less-than (get-delegate this) lower-bound))

   (define (sorted-map-entry-at-least this lower-bound)
     (generic-sorted-map-entry-at-most (get-delegate this) lower-bound))

   (define (sorted-map-keys this)
     (sorted-set-reverse (generic-sorted-map-keys (get-delegate this))))

   (define (sorted-map-entries this)
     (sorted-set-reverse (generic-sorted-map-entries (get-delegate this))))

   (define (sorted-submap this key-range)
     (make-reversed-mutable-sorted-map
      (generic-sorted-submap (get-delegate this) (range-reverse key-range))))

   (define (sorted-map-reverse this)
     (get-delegate this))]

  #:methods gen:mutable-sorted-map

  [(define (get-delegate this)
     (reversed-mutable-sorted-map-delegate-map this))

   (define/generic generic-sorted-map-get! sorted-map-get!)
   (define/generic generic-sorted-map-get-entry! sorted-map-get-entry!)
   (define/generic generic-sorted-map-put! sorted-map-put!)
   (define/generic generic-sorted-map-put-all! sorted-map-put-all!)
   (define/generic generic-sorted-map-put-if-absent! sorted-map-put-if-absent!)
   (define/generic generic-sorted-map-update! sorted-map-update!)
   (define/generic generic-sorted-map-remove! sorted-map-remove!)
   (define/generic generic-sorted-map-remove-all! sorted-map-remove-all!)
   (define/generic generic-sorted-map-clear! sorted-map-clear!)

   (define (sorted-map-get!
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get!) key this)])
     (generic-sorted-map-get! (get-delegate this) key failure-result))

   (define (sorted-map-get-entry!
            this
            key
            [failure-result
              (default-sorted-map-lookup-failure-result (name sorted-map-get-entry!) key this)])
     (generic-sorted-map-get-entry! (get-delegate this) key failure-result))

   (define (sorted-map-put! this key value)
     (generic-sorted-map-put! (get-delegate this) key value))

   (define (sorted-map-put-all! this entries)
     (generic-sorted-map-put-all! (get-delegate this) entries))

   (define (sorted-map-put-if-absent! this key value)
     (generic-sorted-map-put-if-absent! (get-delegate this) key value))

   (define (sorted-map-update!
            this
            key
            updater
            [failure-result
              (default-sorted-map-lookup-failure-result (name sorted-map-update!) key this)])
     (generic-sorted-map-update! (get-delegate this) key updater failure-result))

   (define (sorted-map-remove! this key)
     (generic-sorted-map-remove! (get-delegate this) key))

   (define (sorted-map-remove-all! this keys)
     (generic-sorted-map-remove-all! (get-delegate this) keys))

   (define (sorted-map-clear! this)
     (generic-sorted-map-clear! (get-delegate this)))])


(define (range-reverse original-range)
  (range
   (range-upper-bound original-range)
   (range-lower-bound original-range)
   #:comparator (comparator-reverse (range-comparator original-range))))
