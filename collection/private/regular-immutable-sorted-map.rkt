#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-map->immutable-sorted-map (-> sorted-map? immutable-sorted-map?)]))


(module+ private-for-rebellion-only
  (provide
   (contract-out
    [regular-immutable-sorted-map? predicate/c]
    [make-regular-immutable-sorted-map
     (-> (sequence/c entry?) comparator? immutable-sorted-map?)]
    [constructor:regular-immutable-sorted-map
     (-> (and/c vector? immutable?) (and/c vector? immutable?) comparator? immutable-sorted-map?)])))


(require racket/contract/combinator
         racket/match
         racket/generic
         racket/sequence
         racket/stream
         racket/unsafe/ops
         racket/vector
         rebellion/base/comparator
         rebellion/base/option
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/entry
         (submod rebellion/collection/private/persistent-sorted-map private-for-rebellion-only)
         (submod rebellion/collection/private/regular-immutable-sorted-set private-for-rebellion-only)
         rebellion/collection/private/reversed-sorted-map
         rebellion/collection/private/sorted-map-entry-set
         rebellion/collection/private/sorted-map-key-set
         rebellion/collection/private/sorted-map-interface
         (submod rebellion/collection/private/sorted-map-interface private-for-rebellion-only)
         rebellion/collection/private/vector-binary-search
         rebellion/private/cut
         rebellion/private/guarded-block
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


(define (sorted-map->immutable-sorted-map map)
  (cond
    [(regular-immutable-sorted-map? map) map]
    [(regular-immutable-sorted-submap? map)
     (define start (regular-immutable-sorted-submap-start-index map))
     (define end (regular-immutable-sorted-submap-end-index))
     (define keys (make-vector (- end start)))
     (define values (make-vector (- end start)))
     (vector-copy! keys (regular-immutable-sorted-submap-sorted-key-vector map) start end)
     (vector-copy! values (regular-immutable-sorted-submap-sorted-value-vector map) start end)
     (constructor:regular-immutable-sorted-map
      (unsafe-vector*->immutable-vector! keys)
      (unsafe-vector*->immutable-vector! values)
      (sorted-map-key-comparator map))]
    [else
     (define size (sorted-map-size map))
     (define keys
       (unsafe-vector*->immutable-vector!
        (for/vector #:length size ([k (in-sorted-map-keys map)])
          k)))
     (define values
       (unsafe-vector*->immutable-vector!
        (for/vector #:length size ([v (in-sorted-map-values map)])
          v)))
     (constructor:regular-immutable-sorted-map keys values (sorted-map-key-comparator map))]))


(define (make-regular-immutable-sorted-map entries key-comparator)
  (define entry-vec
    (for/vector ([e entries])
      e))
  (vector-sort! entry-vec (λ (a b) (compare-infix key-comparator a < b)) #:key entry-key)
  ;; TODO: check for duplicate keys.
  (define values-vec
    (for/vector #:length (vector-length entry-vec) ([e (in-vector entry-vec)])
      (entry-value e)))
  (for ([i (in-range 0 (vector-length entry-vec))])
    (vector-set! entry-vec i (entry-key (vector-ref entry-vec i))))
  (constructor:regular-immutable-sorted-map
   (unsafe-vector*->immutable-vector! entry-vec)
   (unsafe-vector*->immutable-vector! values-vec)
   key-comparator))


(struct abstract-regular-immutable-sorted-map abstract-immutable-sorted-map ()

  #:methods gen:immutable-sorted-map

  [(define/generic generic-sorted-map-put sorted-map-put)
   (define/generic generic-sorted-map-put-all sorted-map-put-all)
   (define/generic generic-sorted-map-put-if-absent sorted-map-put-if-absent)
   (define/generic generic-sorted-map-update sorted-map-update)
   (define/generic generic-sorted-map-remove sorted-map-remove)
   (define/generic generic-sorted-map-remove-all sorted-map-remove-all)

   (define (sorted-map-put this key value)
     (generic-sorted-map-put (sorted-map->persistent-sorted-map this) key value))

   (define (sorted-map-put-all this entries)
     (generic-sorted-map-put-all (sorted-map->persistent-sorted-map this) entries))

   (define (sorted-map-put-if-absent this key value)
     (generic-sorted-map-put-if-absent (sorted-map->persistent-sorted-map this) key value))

   (define (sorted-map-update
            this
            key
            updater
            [failure-result
             (default-sorted-map-lookup-failure-result (name sorted-map-update) key this)])
     (generic-sorted-map-update (sorted-map->persistent-sorted-map this) key updater failure-result))

   (define (sorted-map-remove this key)
     (generic-sorted-map-remove (sorted-map->persistent-sorted-map this) key))

   (define (sorted-map-remove-all this keys)
     (generic-sorted-map-remove-all (sorted-map->persistent-sorted-map this) keys))])


(struct regular-immutable-sorted-map abstract-regular-immutable-sorted-map

  (sorted-key-vector sorted-value-vector key-comparator)

  #:constructor-name constructor:regular-immutable-sorted-map

  #:methods gen:sorted-map

  [(define (in-sorted-map this #:descending? [descending? #false])
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (if descending?
         (for/stream ([k (in-vector keys (sub1 (vector-length keys)) -1 -1)]
                      [v (in-vector values (sub1 (vector-length values)) -1 -1)])
           (entry k v))
         (for/stream ([k (in-vector keys)]
                      [v (in-vector values)])
           (entry k v))))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (if descending?
         (in-vector keys (sub1 (vector-length keys)) -1 -1)
         (in-vector keys)))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (if descending?
         (in-vector values (sub1 (vector-length values)) -1 -1)
         (in-vector values)))

   (define (sorted-map-empty? this)
     (vector-empty? (regular-immutable-sorted-map-sorted-key-vector this)))

   (define (sorted-map-size this)
     (vector-length (regular-immutable-sorted-map-sorted-key-vector this)))

   (define (sorted-map-key-comparator this)
     (regular-immutable-sorted-map-key-comparator this))

   (define/guard (sorted-map-contains-key? this key)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (and (contract-first-order-passes? (comparator-operand-contract key<=>) key)
          (list-position? (vector-binary-search keys key #:comparator key<=>))))

   (define/guard (sorted-map-contains-entry? this e)
     (match-define (entry key value) e)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (guard (contract-first-order-passes? (comparator-operand-contract key<=>) key) else
       #false)
     (match (vector-binary-search keys key #:comparator key<=>)
       [(list-position i _) (equal? (vector-ref values i) value)]
       [_ #false]))

   (define/guard (sorted-map-contains-value? this value)
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (for/or ([v (in-vector values)])
       (equal? v value)))

   (define/guard (sorted-map-get
                  this
                  key
                  [failure-result
                   (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (guard (contract-first-order-passes? (comparator-operand-contract key<=>) key) else
       (if (procedure? failure-result) (failure-result) failure-result))
     (match (vector-binary-search keys key #:comparator key<=>)
       [(list-position i _) (vector-ref values i)]
       [_ (if (procedure? failure-result) (failure-result) failure-result)]))

   (define/guard (sorted-map-get-option this key)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (guard (contract-first-order-passes? (comparator-operand-contract key<=>) key) else
       absent)
     (match (vector-binary-search keys key #:comparator key<=>)
       [(list-position i _) (present (vector-ref values i))]
       [_ absent]))

   (define/guard (sorted-map-get-entry
                  this
                  key
                  [failure-result
                   (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (guard (contract-first-order-passes? (comparator-operand-contract key<=>) key) else
       (entry key (if (procedure? failure-result) (failure-result) failure-result)))
     (match (vector-binary-search keys key #:comparator key<=>)
       [(list-position i real-key) (entry real-key (vector-ref values i))]
       [_ (entry key (if (procedure? failure-result) (failure-result) failure-result))]))

   (define (sorted-map-least-key this)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (if (vector-empty? keys) absent (present (vector-ref keys 0))))

   (define (sorted-map-least-entry this)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (if (vector-empty? keys) absent (present (entry (vector-ref keys 0) (vector-ref values 0)))))

   (define (sorted-map-greatest-key this)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (if (vector-empty? keys) absent (present (vector-ref keys (sub1 (vector-length keys))))))

   (define (sorted-map-greatest-entry this)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define last-index (sub1 (vector-length keys)))
     (if (negative? last-index)
         absent
         (present (entry (vector-ref keys last-index) (vector-ref values last-index)))))

   (define (sorted-map-key-less-than this upper-bound)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (vector-binary-search-element-less-than keys upper-bound #:comparator key<=>))

   (define (sorted-map-entry-less-than this upper-bound)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (define search (vector-binary-search-cut keys (lower-cut upper-bound) #:comparator key<=>))
     (option-map
      (list-gap-index-before search) (λ (i) (entry (vector-ref keys i) (vector-ref values i)))))

   (define (sorted-map-key-greater-than this lower-bound)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (vector-binary-search-element-greater-than keys lower-bound #:comparator key<=>))

   (define (sorted-map-entry-greater-than this lower-bound)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (define search (vector-binary-search-cut keys (upper-cut lower-bound) #:comparator key<=>))
     (option-map
      (list-gap-index-after search) (λ (i) (entry (vector-ref keys i) (vector-ref values i)))))

   (define (sorted-map-key-at-most this upper-bound)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (vector-binary-search-element-at-most keys upper-bound #:comparator key<=>))

   (define (sorted-map-entry-at-most this upper-bound)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (define search (vector-binary-search-cut keys (upper-cut upper-bound) #:comparator key<=>))
     (option-map
      (list-gap-index-before search) (λ (i) (entry (vector-ref keys i) (vector-ref values i)))))

   (define (sorted-map-key-at-least this lower-bound)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (vector-binary-search-element-at-least keys lower-bound #:comparator key<=>))

   (define (sorted-map-entry-at-least this lower-bound)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (define search (vector-binary-search-cut keys (lower-cut lower-bound) #:comparator key<=>))
     (option-map
      (list-gap-index-after search) (λ (i) (entry (vector-ref keys i) (vector-ref values i)))))

   (define (sorted-map-keys this)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (make-regular-immutable-sorted-set keys key<=>))

   (define (sorted-map-entries this)
     (make-immutable-sorted-map-entry-set this))

   (define (sorted-submap this element-range)
     (define keys (regular-immutable-sorted-map-sorted-key-vector this))
     (define values (regular-immutable-sorted-map-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-map-key-comparator this))
     (define start
       (list-gap-index
        (vector-binary-search-cut keys (range-lower-cut element-range) #:comparator key<=>)))
     (define end
       (list-gap-index
        (vector-binary-search-cut keys (range-upper-cut element-range) #:comparator key<=>)))
     (cond
       [(equal? start end) (empty-sorted-map key<=>)]
       [(and (zero? start) (equal? end (vector-length keys))) this]
       [else (constructor:regular-immutable-sorted-submap keys values key<=> start end)]))

   (define (sorted-map-reverse this)
     (make-reversed-immutable-sorted-map this))])


(struct regular-immutable-sorted-submap abstract-regular-immutable-sorted-map

  (sorted-key-vector sorted-value-vector key-comparator start-index end-index)

  #:constructor-name constructor:regular-immutable-sorted-submap

  #:methods gen:sorted-map

  [(define (in-sorted-map this #:descending? [descending? #false])
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (if descending?
         (for/stream ([k (in-vector keys (sub1 end) (sub1 start) -1)]
                      [v (in-vector values (sub1 end) (sub1 start) -1)])
           (entry k v))
         (for/stream ([k (in-vector keys start end)]
                      [v (in-vector values start end)])
           (entry k v))))

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (if descending?
         (in-vector keys (sub1 end) (sub1 start) -1)
         (in-vector keys start end)))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (if descending?
         (in-vector values (sub1 end) (sub1 start) -1)
         (in-vector values start end)))

   (define (sorted-map-empty? this)
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (equal? start end))

   (define (sorted-map-size this)
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (- end start))

   (define (sorted-map-key-comparator this)
     (regular-immutable-sorted-submap-key-comparator this))

   (define (sorted-map-contains-key? this key)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (and (contract-first-order-passes? (comparator-operand-contract key<=>) key)
          (list-position? (vector-binary-search keys key start end #:comparator key<=>))))

   (define (sorted-map-contains-value? this value)
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (for/or ([v (in-vector values start end)])
       (equal? v value)))

   (define/guard (sorted-map-contains-entry? this e)
     (match-define (entry key value) e)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (guard (contract-first-order-passes? (comparator-operand-contract key<=>) key) else
       #false)
     (match (vector-binary-search keys key start end #:comparator key<=>)
       [(list-position i _) (equal? (vector-ref values i) value)]
       [_ #false]))

   (define/guard (sorted-map-get
                  this
                  key
                  [failure-result
                   (default-sorted-map-lookup-failure-result (name sorted-map-get) key this)])
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (guard (contract-first-order-passes? (comparator-operand-contract key<=>) key) else
       (if (procedure? failure-result) (failure-result) failure-result))
     (match (vector-binary-search keys key start end #:comparator key<=>)
       [(list-position i _) (vector-ref values i)]
       [_ (if (procedure? failure-result) (failure-result) failure-result)]))

   (define/guard (sorted-map-get-option this key)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (guard (contract-first-order-passes? (comparator-operand-contract key<=>) key) else
       absent)
     (match (vector-binary-search keys key start end #:comparator key<=>)
       [(list-position i _) (present (vector-ref values i))]
       [_ absent]))

   (define/guard (sorted-map-get-entry
                  this
                  key
                  [failure-result
                   (default-sorted-map-lookup-failure-result (name sorted-map-get-entry) key this)])
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (guard (contract-first-order-passes? (comparator-operand-contract key<=>) key) else
       (entry key (if (procedure? failure-result) (failure-result) failure-result)))
     (match (vector-binary-search keys key start end #:comparator key<=>)
       [(list-position i real-key) (entry real-key (vector-ref values i))]
       [_ (entry key (if (procedure? failure-result) (failure-result) failure-result))]))

   (define/guard (sorted-map-least-key this)
     (guard (sorted-map-empty? this) then
       absent)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (present (vector-ref keys start)))

   (define/guard (sorted-map-least-entry this)
     (guard (sorted-map-empty? this) then
       absent)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (present (entry (vector-ref keys start) (vector-ref values start))))

   (define (sorted-map-greatest-key this)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (if (zero? end) absent (present (vector-ref keys (sub1 end)))))

   (define (sorted-map-greatest-entry this)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (if (zero? end)
         absent
         (present (entry (vector-ref keys (sub1 end)) (vector-ref values (sub1 end))))))

   (define (sorted-map-key-less-than this upper-bound)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (vector-binary-search-element-less-than keys upper-bound start end #:comparator key<=>))

   (define (sorted-map-entry-less-than this upper-bound)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (define search
       (vector-binary-search-cut keys (lower-cut upper-bound) start end #:comparator key<=>))
     (option-map
      (list-gap-index-before search) (λ (i) (entry (vector-ref keys i) (vector-ref values i)))))

   (define (sorted-map-key-greater-than this lower-bound)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (vector-binary-search-element-greater-than keys lower-bound start end #:comparator key<=>))

   (define (sorted-map-entry-greater-than this lower-bound)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (define search
       (vector-binary-search-cut keys (upper-cut lower-bound) start end #:comparator key<=>))
     (option-map
      (list-gap-index-after search) (λ (i) (entry (vector-ref keys i) (vector-ref values i)))))

   (define (sorted-map-key-at-most this upper-bound)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (vector-binary-search-element-at-most keys upper-bound start end #:comparator key<=>))

   (define (sorted-map-entry-at-most this upper-bound)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (define search
       (vector-binary-search-cut keys (upper-cut upper-bound) start end #:comparator key<=>))
     (option-map
      (list-gap-index-before search) (λ (i) (entry (vector-ref keys i) (vector-ref values i)))))

   (define (sorted-map-key-at-least this lower-bound)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (vector-binary-search-element-at-least keys lower-bound start end #:comparator key<=>))

   (define (sorted-map-entry-at-least this lower-bound)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (define search
       (vector-binary-search-cut keys (lower-cut lower-bound) start end #:comparator key<=>))
     (option-map
      (list-gap-index-after search) (λ (i) (entry (vector-ref keys i) (vector-ref values i)))))

   (define (sorted-map-keys this)
     (make-immutable-sorted-map-key-set this))

   (define (sorted-map-entries this)
     (make-immutable-sorted-map-entry-set this))

   (define (sorted-submap this element-range)
     (define keys (regular-immutable-sorted-submap-sorted-key-vector this))
     (define values (regular-immutable-sorted-submap-sorted-value-vector this))
     (define key<=> (regular-immutable-sorted-submap-key-comparator this))
     (define start (regular-immutable-sorted-submap-start-index this))
     (define end (regular-immutable-sorted-submap-end-index this))
     (define new-start
       (list-gap-index
        (vector-binary-search-cut
         keys (range-lower-cut element-range) start end #:comparator key<=>)))
     (define new-end
       (list-gap-index
        (vector-binary-search-cut
         keys (range-upper-cut element-range) start end #:comparator key<=>)))
     (if (equal? new-start new-end)
         (empty-sorted-map key<=>)
         (constructor:regular-immutable-sorted-submap keys values key<=> new-start new-end)))

   (define (sorted-map-reverse this)
     (make-reversed-immutable-sorted-map this))])