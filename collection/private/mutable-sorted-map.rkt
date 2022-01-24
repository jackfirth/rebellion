#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-mutable-sorted-map
   (->* (#:key-comparator comparator?) ((sequence/c entry?)) mutable-sorted-map?)]))


(require racket/generic
         racket/match
         racket/sequence
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/private/mutable-red-black-tree
         (submod rebellion/collection/private/mutable-sorted-set private-for-rebellion-only)
         rebellion/collection/private/reversed-sorted-map
         rebellion/collection/private/sorted-map-entry-set
         (submod rebellion/collection/private/sorted-map-entry-set private-for-rebellion-only)
         rebellion/collection/private/sorted-map-interface
         (submod rebellion/collection/private/sorted-map-interface private-for-rebellion-only)
         rebellion/collection/private/sorted-map-key-set
         rebellion/collection/private/sorted-submap
         rebellion/collection/vector
         rebellion/private/guarded-block
         rebellion/private/precondition
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(struct regular-mutable-sorted-map abstract-mutable-sorted-map (tree)

  #:constructor-name constructor:regular-mutable-sorted-map

  #:methods gen:sorted-map

  [(define (in-sorted-map this #:descending? [descending? #false])
     (in-mutable-rb-tree (regular-mutable-sorted-map-tree this) #:descending? descending?))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (in-mutable-rb-tree-keys (regular-mutable-sorted-map-tree this) #:descending? descending?))
   
   (define (in-sorted-map-values this #:descending? [descending? #false])
     (in-mutable-rb-tree-values (regular-mutable-sorted-map-tree this) #:descending? descending?))

   (define (sorted-map-size this)
     (mutable-rb-tree-size (regular-mutable-sorted-map-tree this)))

   (define (sorted-map-key-comparator this)
     (mutable-rb-tree-key-comparator (regular-mutable-sorted-map-tree this)))

   (define (sorted-map-contains-key? this key)
     (mutable-rb-tree-contains-key? (regular-mutable-sorted-map-tree this) key))
   
   (define (sorted-map-contains-value? this value)
     (mutable-rb-tree-contains-value? (regular-mutable-sorted-map-tree this) value))
  
   (define (sorted-map-contains-entry? this entry)
     (mutable-rb-tree-contains-value? (regular-mutable-sorted-map-tree this) entry))
  
   (define (sorted-map-get
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (mutable-rb-tree-get (regular-mutable-sorted-map-tree this) key failure-result))

   (define (sorted-map-get-option this key)
     (mutable-rb-tree-get-option (regular-mutable-sorted-map-tree this) key))
  
   (define (sorted-map-get-entry
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (mutable-rb-tree-get-entry (regular-mutable-sorted-map-tree this) key failure-result))
  
   (define (sorted-map-least-key this)
     (mutable-rb-tree-least-key (regular-mutable-sorted-map-tree this)))
  
   (define (sorted-map-greatest-key this)
     (mutable-rb-tree-greatest-key (regular-mutable-sorted-map-tree this)))
  
   (define (sorted-map-key-less-than this upper-bound)
     (mutable-rb-tree-key-less-than (regular-mutable-sorted-map-tree this) upper-bound))
  
   (define (sorted-map-key-at-most this upper-bound)
     (mutable-rb-tree-key-at-most (regular-mutable-sorted-map-tree this) upper-bound))
  
   (define (sorted-map-key-greater-than this lower-bound)
     (mutable-rb-tree-key-greater-than (regular-mutable-sorted-map-tree this) lower-bound))
  
   (define (sorted-map-key-at-least this lower-bound)
     (mutable-rb-tree-key-at-least (regular-mutable-sorted-map-tree this) lower-bound))
  
   (define (sorted-map-least-entry this)
     (mutable-rb-tree-least-entry (regular-mutable-sorted-map-tree this)))
  
   (define (sorted-map-greatest-entry this)
     (mutable-rb-tree-greatest-entry (regular-mutable-sorted-map-tree this)))
  
   (define (sorted-map-entry-less-than this upper-key-bound)
     (mutable-rb-tree-entry-less-than (regular-mutable-sorted-map-tree this) upper-key-bound))
  
   (define (sorted-map-entry-at-most this upper-key-bound)
     (mutable-rb-tree-entry-at-most (regular-mutable-sorted-map-tree this) upper-key-bound))
  
   (define (sorted-map-entry-greater-than this lower-key-bound)
     (mutable-rb-tree-entry-greater-than (regular-mutable-sorted-map-tree this) lower-key-bound))
  
   (define (sorted-map-entry-at-least this lower-key-bound)
     (mutable-rb-tree-entry-at-least (regular-mutable-sorted-map-tree this) lower-key-bound))
  
   (define (sorted-map-keys this)
     (make-mutable-sorted-map-key-set this))
  
   (define (sorted-map-entries this)
     (make-mutable-sorted-map-entry-set this))
  
   (define (sorted-submap this key-range)
     (constructor:regular-mutable-sorted-submap this key-range))
  
   (define (sorted-map-reverse this)
     (make-reversed-mutable-sorted-map this))]

  #:methods gen:mutable-sorted-map

  [(define (sorted-map-get! this key failure-result)
     (mutable-rb-tree-get! (regular-mutable-sorted-map-tree this) key failure-result))
   
   (define (sorted-map-get-entry! this key failure-result)
     (mutable-rb-tree-get-entry! (regular-mutable-sorted-map-tree this) key failure-result))
   
   (define (sorted-map-put! this key value)
     (mutable-rb-tree-put! (regular-mutable-sorted-map-tree this) key value))
   
   (define (sorted-map-put-all! this entries)
     (mutable-rb-tree-put-all!
      (regular-mutable-sorted-map-tree this) entries #:who (name sorted-map-put-all!)))
   
   (define (sorted-map-put-if-absent! this key value)
     (mutable-rb-tree-put-if-absent! (regular-mutable-sorted-map-tree this) key value))
   
   (define (sorted-map-update!
            this
            key
            updater
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-update!) key this)])
     (mutable-rb-tree-update! (regular-mutable-sorted-map-tree this) key updater failure-result))

   (define (sorted-map-remove! this key)
     (mutable-rb-tree-remove! (regular-mutable-sorted-map-tree this) key))
   
   (define (sorted-map-clear! this)
     (mutable-rb-tree-clear! (regular-mutable-sorted-map-tree this)))])


(struct regular-mutable-sorted-submap abstract-mutable-sorted-map (delegate-map key-range)

  #:constructor-name constructor:regular-mutable-sorted-submap
  
  #:methods gen:sorted-map

  [(define (get-delegate this)
     (regular-mutable-sorted-submap-delegate-map this))

   (define (get-tree this)
     (regular-mutable-sorted-map-tree (get-delegate this)))

   (define (get-range this)
     (regular-mutable-sorted-submap-key-range this))

   (define/generic generic-sorted-map-key-comparator sorted-map-key-comparator)

   (define (in-sorted-map this #:descending? [descending? #false])
     (in-mutable-rb-subtree (get-tree this) (get-range this) #:descending? descending?))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (in-mutable-rb-subtree-keys (get-tree this) (get-range this) #:descending? descending?))
   
   (define (in-sorted-map-values this #:descending? [descending? #false])
     (in-mutable-rb-subtree-values (get-tree this) (get-range this) #:descending? descending?))

   (define (sorted-map-size this)
     (mutable-rb-subtree-size (get-tree this) (get-range this)))

   (define (sorted-map-key-comparator this)
     (mutable-rb-tree-key-comparator (get-tree this)))

   (define (sorted-map-contains-key? this key)
     (sorted-submap-contains-key? (get-delegate this) (get-range this) key))
   
   (define (sorted-map-contains-value? this value)
     (for/or ([v (in-mutable-rb-subtree-values (get-tree this) (get-range this))])
       (equal? v value)))
  
   (define (sorted-map-contains-entry? this entry)
     (sorted-submap-contains-entry? (get-delegate this) (get-range this) entry))
  
   (define (sorted-map-get
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (sorted-submap-get (get-delegate this) (get-range this) key failure-result))

   (define (sorted-map-get-option this key)
     (sorted-submap-get-option (get-delegate this) (get-range this) key))
  
   (define (sorted-map-get-entry
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (sorted-submap-get-entry (get-delegate this) (get-range this) key failure-result))
  
   (define (sorted-map-least-key this)
     (sorted-submap-least-key (get-delegate this) (get-range this)))
  
   (define (sorted-map-greatest-key this)
     (sorted-submap-greatest-key (get-delegate this) (get-range this)))
  
   (define (sorted-map-key-less-than this upper-bound)
     (sorted-submap-key-less-than (get-delegate this) (get-range this) upper-bound))
  
   (define (sorted-map-key-at-most this upper-bound)
     (sorted-submap-key-at-most (get-delegate this) (get-range this) upper-bound))
  
   (define (sorted-map-key-greater-than this lower-bound)
     (sorted-submap-key-greater-than (get-delegate this) (get-range this) lower-bound))
  
   (define (sorted-map-key-at-least this lower-bound)
     (sorted-submap-key-at-least (get-delegate this) (get-range this) lower-bound))
  
   (define (sorted-map-least-entry this)
     (sorted-submap-least-entry (get-delegate this) (get-range this)))
  
   (define (sorted-map-greatest-entry this)
     (sorted-submap-greatest-entry (get-delegate this) (get-range this)))
  
   (define (sorted-map-entry-less-than this upper-key-bound)
     (sorted-submap-entry-less-than (get-delegate this) (get-range this) upper-key-bound))
  
   (define (sorted-map-entry-at-most this upper-key-bound)
     (sorted-submap-entry-at-most (get-delegate this) (get-range this) upper-key-bound))
  
   (define (sorted-map-entry-greater-than this lower-key-bound)
     (sorted-submap-entry-greater-than (get-delegate this) (get-range this) lower-key-bound))
  
   (define (sorted-map-entry-at-least this lower-key-bound)
     (sorted-submap-entry-at-least (get-delegate this) (get-range this) lower-key-bound))
  
   (define (sorted-map-keys this)
     (make-mutable-sorted-map-key-set this))
  
   (define (sorted-map-entries this)
     (make-mutable-sorted-map-entry-set this))
  
   (define/guard (sorted-submap this key-range)
     (define delegate (get-delegate this))
     (define original-range (get-range this))
     (guard (range-overlaps? original-range key-range) else
       (make-empty-mutable-sorted-map (generic-sorted-map-key-comparator delegate)))
     (define intersection (range-intersection original-range key-range))
     (constructor:regular-mutable-sorted-submap delegate intersection))
  
   (define (sorted-map-reverse this)
     (make-reversed-mutable-sorted-map this))]

  #:methods gen:mutable-sorted-map

  [(define (get-delegate this)
     (regular-mutable-sorted-submap-delegate-map this))

   (define (get-tree this)
     (regular-mutable-sorted-map-tree (get-delegate this)))

   (define (get-range this)
     (regular-mutable-sorted-submap-key-range this))

   (define (check-key-lookup-in-range this key who)
     (define key-range (get-range this))
     (check-precondition
      (range-contains? key-range key)
      who
      "cannot lookup key outside submap key range"
      "key" key
      "submap key range" key-range))

   (define (check-key-insertion-in-range this key value who)
     (define key-range (get-range this))
     (check-precondition
      (range-contains? key-range key)
      who
      "cannot lookup key outside submap key range"
      "key" key
      "submap key range" key-range))

   (define/generic generic-sorted-map-get! sorted-map-get!)
   (define/generic generic-sorted-map-get-entry! sorted-map-get-entry!)
   (define/generic generic-sorted-map-put! sorted-map-put!)
   (define/generic generic-sorted-map-put-all! sorted-map-put-all!)
   (define/generic generic-sorted-map-put-if-absent! sorted-map-put-if-absent!)
   (define/generic generic-sorted-map-update! sorted-map-update!)
   (define/generic generic-sorted-map-remove! sorted-map-remove!)

   (define/guard (sorted-map-get! this key failure-result)
     (check-key-lookup-in-range this key (name sorted-map-get!))
     (generic-sorted-map-get! (get-delegate this) key failure-result))
   
  (define (sorted-map-get-entry! this key failure-result)
    (check-key-lookup-in-range this key (name sorted-map-get-entry!))
    (generic-sorted-map-get-entry! (get-delegate this) key failure-result))

  (define (sorted-map-put! this key value)
    (check-key-insertion-in-range this key value (name sorted-map-put!))
    (generic-sorted-map-put! (get-delegate this) key value))
  
  (define (sorted-map-put-all! this entries)
    (define entry-vector (sequence->vector entries))
    (for ([e (in-vector entry-vector)])
      (match-define (entry key value) e)
      (check-key-insertion-in-range this key value (name sorted-map-put-all!)))
    (generic-sorted-map-put-all! (get-delegate this) entry-vector))
  
  (define (sorted-map-put-if-absent! this key value)
    (check-key-insertion-in-range this key value (name sorted-map-put-if-absent!))
    (generic-sorted-map-put-if-absent! (get-delegate this) key value))
  
  (define (sorted-map-update!
           this
           key
           updater
           [failure-result
            (default-sorted-map-lookup-failure-result (name sorted-map-update!) key this)])
    (check-key-lookup-in-range this key (name sorted-map-update!))
    (generic-sorted-map-update! (get-delegate this) key updater failure-result))
  
  (define (sorted-map-remove! this key)
    (when (range-contains? (get-range this) key)
      (generic-sorted-map-remove! (get-delegate this) key)))
  
  (define (sorted-map-clear! this)
    (mutable-rb-subtree-clear! (get-tree this) (get-range this)))])


(struct empty-mutable-sorted-map abstract-mutable-sorted-map (key-comparator)

  #:constructor-name make-empty-mutable-sorted-map

  #:methods gen:sorted-map

  [(define (in-sorted-map this #:descending? [descending? #false])
     (in-list '()))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (in-list '()))
   
   (define (in-sorted-map-values this #:descending? [descending? #false])
     (in-list '()))

   (define (sorted-map-size this)
     0)

   (define (sorted-map-key-comparator this)
     (empty-mutable-sorted-map-key-comparator this))

   (define (sorted-map-contains-key? this key)
     #false)
   
   (define (sorted-map-contains-value? this value)
     #false)
  
   (define (sorted-map-contains-entry? this entry)
     #false)
  
   (define (sorted-map-get
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (if (procedure? failure-result) (failure-result) failure-result))

   (define (sorted-map-get-option this key)
     absent)
  
   (define (sorted-map-get-entry
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (entry key (if (procedure? failure-result) (failure-result) failure-result)))
  
   (define (sorted-map-least-key this)
     absent)
  
   (define (sorted-map-greatest-key this)
     absent)
  
   (define (sorted-map-key-less-than this upper-bound)
     absent)
  
   (define (sorted-map-key-at-most this upper-bound)
     absent)
  
   (define (sorted-map-key-greater-than this lower-bound)
     absent)
  
   (define (sorted-map-key-at-least this lower-bound)
     absent)
  
   (define (sorted-map-least-entry this)
     absent)
  
   (define (sorted-map-greatest-entry this)
     absent)
  
   (define (sorted-map-entry-less-than this upper-key-bound)
     absent)
  
   (define (sorted-map-entry-at-most this upper-key-bound)
     absent)
  
   (define (sorted-map-entry-greater-than this lower-key-bound)
     absent)
  
   (define (sorted-map-entry-at-least this lower-key-bound)
     absent)
  
   (define (sorted-map-keys this)
     (make-empty-mutable-sorted-set (empty-mutable-sorted-map-key-comparator this)))
  
   (define (sorted-map-entries this)
     (make-empty-mutable-sorted-set
      (make-entry-comparator (empty-mutable-sorted-map-key-comparator this))))
  
   (define (sorted-submap this key-range)
     this)
  
   (define (sorted-map-reverse this)
     (make-empty-mutable-sorted-map
      (comparator-reverse (empty-mutable-sorted-map-key-comparator this))))]

  #:methods gen:mutable-sorted-map

  [(define (sorted-map-get! this key failure-result)
     (raise-arguments-error
      (name sorted-map-get!) "cannot lookup any keys, submap key range is empty" "key" key))
   
   (define (sorted-map-get-entry! this key failure-result)
     (raise-arguments-error
      (name sorted-map-get-entry!) "cannot lookup any keys, submap key range is empty" "key" key))
   
   (define (sorted-map-put! this key value)
     (raise-arguments-error
      (name sorted-map-put!)
      "cannot insert any entries, submap key range is empty"
      "key" key
      "value" value))
   
   (define (sorted-map-put-all! this entries)
     (raise-arguments-error
      (name sorted-map-put-all!)
      "cannot insert any entries, submap key range is empty"
      "entries" entries))
   
   (define (sorted-map-put-if-absent! this key value)
     (raise-arguments-error
      (name sorted-map-put-if-absent!)
      "cannot insert any entries, submap key range is empty"
      "key" key
      "value" value))
   
   (define (sorted-map-update!
            this
            key
            updater
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-update!) key this)])
     (raise-arguments-error
      (name sorted-map-update!) "cannot update any entries, submap key range is empty" "key" key))

   (define (sorted-map-remove! this key)
     (void))
   
   (define (sorted-map-clear! this)
     (void))])


(define (make-mutable-sorted-map [entries '()] #:key-comparator key-comparator)
  (define map (constructor:regular-mutable-sorted-map (make-mutable-rb-tree key-comparator)))
  (sorted-map-put-all! map entries)
  map)
