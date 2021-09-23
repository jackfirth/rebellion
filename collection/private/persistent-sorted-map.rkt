#lang racket/base


(require racket/contract/base)


(module+ private-for-rebellion-only
  (provide
   (contract-out
    [empty-sorted-map (-> comparator? immutable-sorted-map?)])))


(require racket/generic
         racket/match
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/base/result
         rebellion/collection/entry
         rebellion/collection/private/persistent-red-black-tree
         (submod rebellion/collection/private/persistent-sorted-set private-for-rebellion-only)
         rebellion/collection/private/reversed-sorted-map
         rebellion/collection/private/sorted-map-interface
         (submod rebellion/collection/private/sorted-map-interface private-for-rebellion-only)
         rebellion/collection/private/sorted-map-entry-set
         rebellion/collection/private/sorted-map-key-set
         rebellion/collection/private/sorted-submap
         rebellion/private/guarded-block
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


;; We define a specialized implementation of the empty map for speed. It's included in this module so
;; that the empty map implementation can switch to the persistent implementation when entries are
;; added to it. The persistent implementation also depends on the empty implementation, since it can
;; switch back to it if an empty submap of the persistent implementation is selected.
(struct empty-sorted-map abstract-immutable-sorted-map (key-comparator)

  #:methods gen:sorted-map

  [(define (in-sorted-map this #:descending? [descending? #false])
     (in-list '()))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (in-list '()))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (in-list '()))

   (define (sorted-map-empty? this)
     #true)

   (define (sorted-map-size this)
     0)

   (define (sorted-map-key-comparator this)
     (empty-sorted-map-key-comparator this))

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

   (define (sorted-map-key-greater-than this lower-bound)
     absent)

   (define (sorted-map-key-at-most this upper-bound)
     absent)

   (define (sorted-map-key-at-least this lower-bound)
     absent)

   (define (sorted-map-least-entry this)
     absent)

   (define (sorted-map-greatest-entry this)
     absent)

   (define (sorted-map-entry-less-than this upper-bound)
     absent)

   (define (sorted-map-entry-greater-than this lower-bound)
     absent)

   (define (sorted-map-entry-at-most this upper-bound)
     absent)

   (define (sorted-map-entry-at-least this lower-bound)
     absent)

   (define (sorted-map-keys this)
     (empty-sorted-set (empty-sorted-map-key-comparator this)))

   (define (sorted-map-entries this)
     (empty-sorted-set (comparator-map (empty-sorted-map-key-comparator this) entry-key)))

   (define (sorted-submap this key-range)
     this)

   (define (sorted-map-reverse this)
     (empty-sorted-map (comparator-reverse (empty-sorted-map-key-comparator this))))]

  #:methods gen:immutable-sorted-map

  [(define (sorted-map-put this key value)
     (define comparator (empty-sorted-map-key-comparator this))
     (make-persistent-sorted-map (list (entry key value)) #:comparator comparator))

   (define (sorted-map-put-all this entries)
     (make-persistent-sorted-map entries #:comparator (empty-sorted-map-key-comparator this)))

   (define (sorted-map-put-if-absent this key value)
     (success (sorted-map-put this key value)))

   (define (sorted-map-update
            this
            key
            updater
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-update) key this)])
     (define value (if (procedure? failure-result) (failure-result) failure-result))
     (define comparator (empty-sorted-map-key-comparator this))
     (make-persistent-sorted-map (list (entry key (updater value))) #:comparator comparator))

   (define (sorted-map-remove this key)
     this)

   (define (sorted-map-remove-all this keys)
     this)])


(define (make-persistent-sorted-map entries #:comparator comparator)
  (constructor:persistent-sorted-map
   (for/fold ([tree (empty-persistent-red-black-tree comparator)])
             ([e entries])
     (persistent-red-black-tree-insert tree (entry-key e) (entry-value e)))))


(struct persistent-sorted-map abstract-immutable-sorted-map (tree)

  #:constructor-name constructor:persistent-sorted-map

  #:methods gen:sorted-map

  [(define (in-sorted-map this #:descending? [descending? #false])
     (in-persistent-red-black-tree (persistent-sorted-map-tree this) #:descending? descending?))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (in-persistent-red-black-tree-keys (persistent-sorted-map-tree this) #:descending? descending?))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (in-persistent-red-black-tree-values
      (persistent-sorted-map-tree this) #:descending? descending?))

   (define (sorted-map-size this)
     (persistent-red-black-tree-size (persistent-sorted-map-tree this)))

   (define (sorted-map-key-comparator this)
     (persistent-red-black-tree-comparator (persistent-sorted-map-tree this)))

   (define (sorted-map-contains-key? this key)
     (persistent-red-black-tree-contains? (persistent-sorted-map-tree this) key))

   (define (sorted-map-contains-value? this value)
     (for/or ([v (in-persistent-red-black-tree-values (persistent-sorted-map-tree this))])
       (equal? v value)))

   (define (sorted-map-contains-entry? this entry)
     (match (persistent-red-black-tree-get-option (persistent-sorted-map-tree this) (entry-key entry))
       [(== absent) #false]
       [(present v) (equal? v (entry-value entry))]))

   (define (sorted-map-get
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (persistent-red-black-tree-get (persistent-sorted-map-tree this) key failure-result))

   (define (sorted-map-get-option this key)
     (persistent-red-black-tree-get-option (persistent-sorted-map-tree this) key))

   (define (sorted-map-get-entry
            this
            key
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (persistent-red-black-tree-get-entry (persistent-sorted-map-tree this) key failure-result))

   (define (sorted-map-least-key this)
     (persistent-red-black-tree-least-key (persistent-sorted-map-tree this)))

   (define (sorted-map-greatest-key this)
     (persistent-red-black-tree-greatest-key (persistent-sorted-map-tree this)))

   (define (sorted-map-key-less-than this upper-bound)
     (persistent-red-black-tree-key-less-than (persistent-sorted-map-tree this) upper-bound))

   (define (sorted-map-key-greater-than this lower-bound)
     (persistent-red-black-tree-key-greater-than (persistent-sorted-map-tree this) lower-bound))

   (define (sorted-map-key-at-most this upper-bound)
     (persistent-red-black-tree-key-at-most (persistent-sorted-map-tree this) upper-bound))
   
   (define (sorted-map-key-at-least this lower-bound)
     (persistent-red-black-tree-key-at-least (persistent-sorted-map-tree this) lower-bound))

   (define (sorted-map-least-entry this)
     (persistent-red-black-tree-least-entry (persistent-sorted-map-tree this)))

   (define (sorted-map-greatest-entry this)
     (persistent-red-black-tree-greatest-entry (persistent-sorted-map-tree this)))

   (define (sorted-map-entry-less-than this upper-key-bound)
     (persistent-red-black-tree-entry-less-than (persistent-sorted-map-tree this) upper-key-bound))

   (define (sorted-map-entry-at-most this upper-key-bound)
     (persistent-red-black-tree-entry-at-most (persistent-sorted-map-tree this) upper-key-bound))

   (define (sorted-map-entry-greater-than this lower-key-bound)
     (persistent-red-black-tree-entry-greater-than (persistent-sorted-map-tree this) lower-key-bound))

   (define (sorted-map-entry-at-least this lower-key-bound)
     (persistent-red-black-tree-entry-at-least (persistent-sorted-map-tree this) lower-key-bound))

   (define (sorted-map-keys this)
     (make-immutable-sorted-map-key-set this))

   (define (sorted-map-entries this)
     (make-immutable-sorted-map-entry-set this))

   (define (sorted-submap this key-range)
     (constructor:persistent-sorted-submap this key-range))

   (define (sorted-map-reverse this)
     (make-reversed-immutable-sorted-map this))]

  #:methods gen:immutable-sorted-map

  [(define (sorted-map-put this key value)
     (define tree (persistent-sorted-map-tree this))
     (constructor:persistent-sorted-map (persistent-red-black-tree-insert tree key value)))

   (define (sorted-map-put-all this entries)
     (define tree
       (for/fold ([tree (persistent-sorted-map-tree this)])
                 ([e entries])
         (persistent-red-black-tree-insert tree (entry-key e) (entry-value e))))
     (constructor:persistent-sorted-map tree))

   (define (sorted-map-put-if-absent this key value)
     (define tree (persistent-sorted-map-tree this))
     (match (persistent-red-black-tree-get-option tree key)
       [(== absent) (success (sorted-map-put this key value))]
       [(present previous-value) (failure previous-value)]))

   (define (sorted-map-update
            this
            key
            updater
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-update) key this)])
     (define tree (persistent-red-black-tree-update tree key updater failure-result))
     (constructor:persistent-sorted-map tree))])


(struct persistent-sorted-submap abstract-immutable-sorted-map (delegate key-range)

  #:constructor-name constructor:persistent-sorted-submap

  #:methods gen:sorted-map

  [(define/generic generic-sorted-map-key-comparator sorted-map-key-comparator)

   (define (get-delegate this)
     (persistent-sorted-submap-delegate this))

   (define (get-tree this)
     (persistent-sorted-map-tree (get-delegate this)))

   (define (get-range this)
     (persistent-sorted-submap-key-range this))

   (define (in-sorted-map this #:descending? [descending? #false])
     (in-persistent-red-black-subtree (get-tree this) (get-range this) #:descending? descending?))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (in-persistent-red-black-subtree-keys
      (get-tree this) (get-range this) #:descending? descending?))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (in-persistent-red-black-subtree-values
      (get-tree this) (get-range this) #:descending? descending?))

   (define (sorted-map-size this)
     (persistent-red-black-subtree-size (get-tree this) (get-range this)))

   (define (sorted-map-key-comparator this)
     (persistent-red-black-tree-comparator (get-tree this)))

   (define (sorted-map-contains-key? this key)
     (sorted-submap-contains-key? (get-delegate this) (get-range this) key))

   (define (sorted-map-contains-value? this value)
     (for/or ([v (in-persistent-red-black-subtree-values (get-tree this) (get-range this))])
       (equal? v value)))

   (define (sorted-map-contains-entry? this e)
     (sorted-submap-contains-entry? (get-delegate this) (get-range this) e))

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

   (define (sorted-map-key-greater-than this lower-bound)
     (sorted-submap-key-greater-than (get-delegate this) (get-range this) lower-bound))

   (define (sorted-map-key-at-most this upper-bound)
     (sorted-submap-key-at-most (get-delegate this) (get-range this) upper-bound))
   
   (define (sorted-map-key-at-least this lower-bound)
     (sorted-submap-key-at-least (get-delegate this) (get-range this) lower-bound))

   (define (sorted-map-least-entry this)
     (sorted-submap-least-entry (get-delegate this) (get-range this)))

   (define (sorted-map-greatest-entry this)
     (sorted-submap-greatest-entry (get-delegate this) (get-range this)))

   (define (sorted-map-entry-less-than this upper-key-bound)
     (sorted-submap-entry-less-than (get-delegate this) (get-range this) upper-key-bound))

   (define (sorted-map-entry-greater-than this lower-key-bound)
     (sorted-submap-entry-greater-than (get-delegate this) (get-range this) lower-key-bound))

   (define (sorted-map-entry-at-most this upper-key-bound)
     (sorted-submap-entry-at-most (get-delegate this) (get-range this) upper-key-bound))

   (define (sorted-map-entry-at-least this lower-key-bound)
     (sorted-submap-entry-at-least (get-delegate this) (get-range this) lower-key-bound))

   (define (sorted-map-keys this)
     (make-immutable-sorted-map-key-set this))

   (define (sorted-map-entries this)
     (make-immutable-sorted-map-entry-set this))

   (define/guard (sorted-submap this key-range)
     (define delegate (get-delegate this))
     (define original-range (get-range this))
     (guard (range-overlaps? original-range key-range) else
       (empty-sorted-map (generic-sorted-map-key-comparator delegate)))
     (define intersection (range-intersection original-range key-range))
     (constructor:persistent-sorted-submap delegate intersection))

   (define (sorted-map-reverse this)
     (make-reversed-immutable-sorted-map this))]

  #:methods gen:immutable-sorted-map

  [(define (sorted-map-put this key value)
     (define tree (persistent-sorted-map-tree this))
     (constructor:persistent-sorted-map (persistent-red-black-tree-insert tree key value)))

   (define (sorted-map-put-all this entries)
     (define tree
       (for/fold ([tree (persistent-sorted-map-tree this)])
                 ([e entries])
         (persistent-red-black-tree-insert tree (entry-key e) (entry-value e))))
     (constructor:persistent-sorted-map tree))

   (define (sorted-map-put-if-absent this key value)
     (define tree (persistent-sorted-map-tree this))
     (match (persistent-red-black-tree-get-option tree key)
       [(== absent) (success (sorted-map-put this key value))]
       [(present previous-value) (failure previous-value)]))

   (define (sorted-map-update
            this
            key
            updater
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-update) key this)])
     (define tree (persistent-red-black-tree-update tree key updater failure-result))
     (constructor:persistent-sorted-map tree))])
