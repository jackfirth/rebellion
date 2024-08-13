#lang racket/base


(require racket/contract/base)


(provide
 for/range-set
 for*/range-set
 (contract-out
  [range-set (->* () (#:comparator comparator?) #:rest (listof nonempty-range?) immutable-range-set?)]
  [make-mutable-range-set
   (->* (#:comparator comparator?) ((sequence/c nonempty-range?)) mutable-range-set?)]
  [sequence->range-set
   (-> (sequence/c nonempty-range?) #:comparator comparator? immutable-range-set?)]
  [into-range-set (-> comparator? (reducer/c nonempty-range? immutable-range-set?))]))


(require (for-syntax racket/base
                     rebellion/private/for-body)
         racket/generic
         (only-in racket/list empty? first)
         racket/match
         racket/sequence
         racket/stream
         (only-in racket/vector vector-sort)
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         (submod rebellion/base/range private-for-rebellion-only)
         rebellion/collection/entry
         rebellion/collection/private/range-set-interface
         (submod rebellion/collection/private/range-set-interface private-for-rebellion-only)
         rebellion/collection/sorted-map
         rebellion/collection/vector
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         (submod rebellion/streaming/reducer private-for-rebellion-only)
         rebellion/streaming/transducer
         rebellion/private/cut
         guard
         rebellion/private/precondition
         rebellion/private/static-name
         rebellion/private/todo
         rebellion/private/vector-merge-adjacent
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(struct immutable-endpoint-map-range-set abstract-immutable-range-set (endpoints comparator)

  #:omit-define-syntaxes

  #:methods gen:range-set

  [(define (this-endpoints this)
     (immutable-endpoint-map-range-set-endpoints this))

   (define (this-comparator this)
     (immutable-endpoint-map-range-set-comparator this))

   (define (in-range-set this #:descending? [descending? #false])
     (define cmp (this-comparator this))
     (for/stream ([endpoint-entry (in-sorted-map (this-endpoints this) #:descending? descending?)])
       (match-define (entry lower upper) endpoint-entry)
       (range-from-cuts lower upper #:comparator cmp)))

   (define (range-set-comparator this)
     (this-comparator this))

   (define (range-set-size this)
     (sorted-map-size (this-endpoints this)))

   (define (range-set-contains? this value)
     (endpoint-map-contains? (this-endpoints this) (this-comparator this) value))

   (define (range-set-encloses? this range)
     (endpoint-map-encloses? (this-endpoints this) (this-comparator this) range))

   (define (range-set-overlaps? this range)
     (endpoint-map-overlaps? (this-endpoints this) (this-comparator this) range))

   (define (range-set-range-containing-or-absent this value)
     (endpoint-map-range-containing-or-absent (this-endpoints this) (this-comparator this) value))

   (define (range-set-span-or-absent this)
     (endpoint-map-span-or-absent (this-endpoints this) (this-comparator this)))

   (define (range-subset this subset-range)
     (define cut-comparator (cut<=> (range-comparator subset-range)))
     (define lower-subset-cut (range-lower-cut subset-range))
     (define upper-subset-cut (range-upper-cut subset-range))
     (define subset-endpoint-range
       (closed-range lower-subset-cut upper-subset-cut #:comparator cut-comparator))

     (define endpoints-submap (sorted-submap (this-endpoints this) subset-endpoint-range))

     (define endpoints-submap-with-left-end-corrected
       (guarded-block
        (guard-match (present (entry leftmost-range-lower-cut leftmost-range-upper-cut))
          (sorted-map-entry-at-most (this-endpoints this) lower-subset-cut)
          #:else
          endpoints-submap)
        (guard (compare-infix cut-comparator leftmost-range-upper-cut > lower-subset-cut) #:else
          endpoints-submap)
        (define corrected-lower-range
          (range-from-cuts lower-subset-cut leftmost-range-upper-cut #:comparator cut-comparator))
        (guard (not (empty-range? corrected-lower-range)) #:else
          endpoints-submap)
        (sorted-map-put endpoints-submap lower-subset-cut leftmost-range-upper-cut)))

     (define endpoints-submap-with-right-end-corrected
       (guarded-block
        (guard-match (present (entry rightmost-range-lower-cut rightmost-range-upper-cut))
          (sorted-map-greatest-entry endpoints-submap-with-left-end-corrected)
          #:else
          endpoints-submap-with-left-end-corrected)
        (define corrected-upper-cut
          (comparator-min cut-comparator rightmost-range-upper-cut upper-subset-cut))
        (define corrected-rightmost-range
          (range-from-cuts rightmost-range-lower-cut corrected-upper-cut #:comparator cut-comparator))
        (guard (not (empty-range? corrected-rightmost-range)) #:else
          (sorted-map-remove endpoints-submap-with-left-end-corrected rightmost-range-lower-cut))
        (sorted-map-put
         endpoints-submap-with-left-end-corrected rightmost-range-lower-cut corrected-upper-cut)))

     (immutable-endpoint-map-range-set
      endpoints-submap-with-right-end-corrected (this-comparator this)))]

  #:methods gen:immutable-range-set

  [(define (this-endpoints this)
     (immutable-endpoint-map-range-set-endpoints this))

   (define (this-comparator this)
     (immutable-endpoint-map-range-set-comparator this))

   (define/guard (range-set-add this range)
     (define cmp (this-comparator this))
     (check-precondition
      (equal? cmp (range-comparator range))
      (name range-set-add)
      "added range does not use the same comparator as the range set"
      "range" range
      "range comparator" (range-comparator range)
      "range set comparator" cmp)
     (guard (not (empty-range? range)) #:else
       this)
     (define endpoints (this-endpoints this))
     (define cut-cmp (sorted-map-key-comparator endpoints))
     (define lower-endpoint-cut (range-lower-cut range))
     (define upper-endpoint-cut (range-upper-cut range))
     (define left-overlapping-range (sorted-map-entry-at-most endpoints lower-endpoint-cut))
     (define right-overlapping-range
       (sorted-map-entry-at-most
        (sorted-submap endpoints (at-least-range lower-endpoint-cut #:comparator cut-cmp))
        upper-endpoint-cut))

     (define new-lower-endpoint
       (match left-overlapping-range
         [(present (entry left-overlapping-lower-endpoint left-overlapping-upper-endpoint))
          (if (compare-infix cut-cmp left-overlapping-upper-endpoint >= lower-endpoint-cut)
              (comparator-min cut-cmp lower-endpoint-cut left-overlapping-lower-endpoint)
              lower-endpoint-cut)]
         [(== absent) lower-endpoint-cut]))
  
     (define new-upper-endpoint
       (match right-overlapping-range
         [(present (entry _ right-overlapping-upper-endpoint))
          (comparator-max cut-cmp upper-endpoint-cut right-overlapping-upper-endpoint)]
         [(== absent) upper-endpoint-cut]))

     (define range-to-remove
       (closed-range new-lower-endpoint new-upper-endpoint #:comparator cut-cmp))
     (define endpoints-to-remove (sorted-map-keys (sorted-submap endpoints range-to-remove)))
     (define new-endpoints
       (sorted-map-put (sorted-map-remove-all endpoints endpoints-to-remove)
                       new-lower-endpoint new-upper-endpoint))
     (immutable-endpoint-map-range-set new-endpoints cmp))

   (define/guard (range-set-remove this range)
     (define cmp (this-comparator this))
     (check-precondition
      (equal? cmp (range-comparator range))
      (name range-set-remove)
      "removed range does not use the same comparator as the range set"
      "range" range
      "range comparator" (range-comparator range)
      "range set comparator" cmp)
     (guard (not (empty-range? range)) #:else
       this)
     (define endpoints (this-endpoints this))
     (define cut-cmp (sorted-map-key-comparator endpoints))
     (define lower-endpoint (range-lower-cut range))
     (define upper-endpoint (range-upper-cut range))
     (define left-overlapping-range (sorted-map-entry-at-most endpoints lower-endpoint))
     (define right-overlapping-range (sorted-map-entry-at-most endpoints upper-endpoint))

     (define lowest-endpoint
       (match left-overlapping-range
         [(present (entry left-overlapping-lower-endpoint left-overlapping-upper-endpoint))
          (if (compare-infix cut-cmp left-overlapping-upper-endpoint >= lower-endpoint)
              (comparator-min cut-cmp lower-endpoint left-overlapping-lower-endpoint)
              lower-endpoint)]
         [(== absent) lower-endpoint]))

     (define new-left-overlapping-endpoints
       (match left-overlapping-range
         [(present (entry left-overlapping-lower-endpoint left-overlapping-upper-endpoint))
          (present
           (entry
            left-overlapping-lower-endpoint
            (comparator-min cut-cmp left-overlapping-upper-endpoint lower-endpoint)))]
         [(== absent) absent]))
  
     (define highest-endpoint
       (match right-overlapping-range
         [(present (entry _ right-overlapping-upper-endpoint))
          (comparator-max cut-cmp upper-endpoint right-overlapping-upper-endpoint)]
         [(== absent) upper-endpoint]))

     (define new-right-overlapping-endpoints
       (match right-overlapping-range
         [(present (entry right-overlapping-lower-endpoint right-overlapping-upper-endpoint))
          (if (compare-infix cut-cmp upper-endpoint < right-overlapping-upper-endpoint)
              (present
               (entry
                (comparator-max cut-cmp right-overlapping-lower-endpoint upper-endpoint)
                right-overlapping-upper-endpoint))
              absent)]
         [(== absent) absent]))

     (define range-to-remove
       (closed-range lowest-endpoint highest-endpoint #:comparator cut-cmp))
     (define endpoints-to-remove (sorted-map-keys (sorted-submap endpoints range-to-remove)))
     (let* ([new-endpoints (sorted-map-remove-all endpoints endpoints-to-remove)]
            [new-endpoints
             (match new-left-overlapping-endpoints
               [(present (entry new-left-lower new-left-upper))
                #:when (not (equal? new-left-lower new-left-upper))
                (sorted-map-put new-endpoints new-left-lower new-left-upper)]
               [_ new-endpoints])]
            [new-endpoints
             (match new-right-overlapping-endpoints
               [(present (entry new-right-lower new-right-upper))
                #:when (not (equal? new-right-lower new-right-upper))
                (sorted-map-put new-endpoints new-right-lower new-right-upper)]
               [_ new-endpoints])])
       (immutable-endpoint-map-range-set new-endpoints cmp)))])


(struct mutable-endpoint-map-range-set abstract-mutable-range-set (endpoints comparator)

  #:omit-define-syntaxes

  #:methods gen:range-set

  [(define (this-endpoints this)
     (mutable-endpoint-map-range-set-endpoints this))

   (define (this-comparator this)
     (mutable-endpoint-map-range-set-comparator this))

   (define (in-range-set this #:descending? [descending? #false])
     (define cmp (this-comparator this))
     (for/stream ([endpoint-entry (in-sorted-map (this-endpoints this) #:descending? descending?)])
       (match-define (entry lower upper) endpoint-entry)
       (range-from-cuts lower upper #:comparator cmp)))

   (define (range-set-comparator this)
     (this-comparator this))

   (define (range-set-size this)
     (sorted-map-size (this-endpoints this)))

   (define (range-set-contains? this value)
     (endpoint-map-contains? (this-endpoints this) (this-comparator this) value))

   (define (range-set-encloses? this range)
     (endpoint-map-encloses? (this-endpoints this) (this-comparator this) range))

   (define (range-set-overlaps? this range)
     (endpoint-map-overlaps? (this-endpoints this) (this-comparator this) range))

   (define (range-set-range-containing-or-absent this value)
     (endpoint-map-range-containing-or-absent (this-endpoints this) (this-comparator this) value))

   (define (range-set-span-or-absent this)
     (endpoint-map-span-or-absent (this-endpoints this) (this-comparator this)))

   (define (range-subset this subset-range)
     (define cmp (this-comparator this))
     (check-precondition
      (equal? cmp (range-comparator subset-range))
      (name range-subset)
      "subset range does not use the same comparator as the range set"
      "subset range" range
      "subset range comparator" (range-comparator subset-range)
      "range set comparator" cmp)
     (mutable-endpoint-submap-range-set this subset-range))]

  #:methods gen:mutable-range-set

  [(define (this-endpoints this)
     (mutable-endpoint-map-range-set-endpoints this))

   (define (this-comparator this)
     (mutable-endpoint-map-range-set-comparator this))

   (define/guard (range-set-add! this range)
     (define cmp (this-comparator this))
     (check-precondition
      (equal? cmp (range-comparator range))
      (name range-set-add!)
      "added range does not use the same comparator as the range set"
      "range" range
      "range comparator" (range-comparator range)
      "range set comparator" cmp)
     (guard (not (empty-range? range)) #:else
       (void))
     (define endpoints (this-endpoints this))
     (define cut-cmp (sorted-map-key-comparator endpoints))
     (define lower-endpoint-cut (range-lower-cut range))
     (define upper-endpoint-cut (range-upper-cut range))
     (define left-overlapping-range (sorted-map-entry-at-most endpoints lower-endpoint-cut))
     (define right-overlapping-range
       (sorted-map-entry-at-most
        (sorted-submap endpoints (at-least-range lower-endpoint-cut #:comparator cut-cmp))
        upper-endpoint-cut))

     (define new-lower-endpoint
       (match left-overlapping-range
         [(present (entry left-overlapping-lower-endpoint left-overlapping-upper-endpoint))
          (if (compare-infix cut-cmp left-overlapping-upper-endpoint >= lower-endpoint-cut)
              (comparator-min cut-cmp lower-endpoint-cut left-overlapping-lower-endpoint)
              lower-endpoint-cut)]
         [(== absent) lower-endpoint-cut]))
  
     (define new-upper-endpoint
       (match right-overlapping-range
         [(present (entry _ right-overlapping-upper-endpoint))
          (comparator-max cut-cmp upper-endpoint-cut right-overlapping-upper-endpoint)]
         [(== absent) upper-endpoint-cut]))

     (define range-to-remove
       (closed-range new-lower-endpoint new-upper-endpoint #:comparator cut-cmp))
     (define endpoints-to-remove
       (sequence->list (sorted-map-keys (sorted-submap endpoints range-to-remove))))
     (sorted-map-remove-all! endpoints endpoints-to-remove)
     (sorted-map-put! endpoints new-lower-endpoint new-upper-endpoint)
     (void))

   (define/guard (range-set-remove! this range)
     (define cmp (this-comparator this))
     (check-precondition
      (equal? cmp (range-comparator range))
      (name range-set-remove!)
      "removed range does not use the same comparator as the range set"
      "range" range
      "range comparator" (range-comparator range)
      "range set comparator" cmp)
     (guard (not (empty-range? range)) #:else
       (void))
     (define endpoints (this-endpoints this))
     (define cut-cmp (sorted-map-key-comparator endpoints))
     (define lower-endpoint (range-lower-cut range))
     (define upper-endpoint (range-upper-cut range))
     (define left-overlapping-range (sorted-map-entry-at-most endpoints lower-endpoint))
     (define right-overlapping-range (sorted-map-entry-at-most endpoints upper-endpoint))

     (define lowest-endpoint
       (match left-overlapping-range
         [(present (entry left-overlapping-lower-endpoint left-overlapping-upper-endpoint))
          (if (compare-infix cut-cmp left-overlapping-upper-endpoint >= lower-endpoint)
              (comparator-min cut-cmp lower-endpoint left-overlapping-lower-endpoint)
              lower-endpoint)]
         [(== absent) lower-endpoint]))

     (define new-left-overlapping-endpoints
       (match left-overlapping-range
         [(present (entry left-overlapping-lower-endpoint left-overlapping-upper-endpoint))
          (present
           (entry
            left-overlapping-lower-endpoint
            (comparator-min cut-cmp left-overlapping-upper-endpoint lower-endpoint)))]
         [(== absent) absent]))
  
     (define highest-endpoint
       (match right-overlapping-range
         [(present (entry _ right-overlapping-upper-endpoint))
          (comparator-max cut-cmp upper-endpoint right-overlapping-upper-endpoint)]
         [(== absent) upper-endpoint]))

     (define new-right-overlapping-endpoints
       (match right-overlapping-range
         [(present (entry right-overlapping-lower-endpoint right-overlapping-upper-endpoint))
          (if (compare-infix cut-cmp upper-endpoint < right-overlapping-upper-endpoint)
              (present
               (entry
                (comparator-max cut-cmp right-overlapping-lower-endpoint upper-endpoint)
                right-overlapping-upper-endpoint))
              absent)]
         [(== absent) absent]))

     (define range-to-remove
       (closed-range lowest-endpoint highest-endpoint #:comparator cut-cmp))
     (define endpoints-to-remove
       (sequence->list (sorted-map-keys (sorted-submap endpoints range-to-remove))))

     (sorted-map-remove-all! endpoints endpoints-to-remove)
     (match new-left-overlapping-endpoints
       [(present (entry new-left-lower new-left-upper))
        #:when (not (equal? new-left-lower new-left-upper))
        (sorted-map-put! endpoints new-left-lower new-left-upper)]
       [_ (void)])
     (match new-right-overlapping-endpoints
       [(present (entry new-right-lower new-right-upper))
        #:when (not (equal? new-right-lower new-right-upper))
        (sorted-map-put! endpoints new-right-lower new-right-upper)]
       [_ (void)])
     (void))

   (define (range-set-clear! this)
     (sorted-map-clear! (this-endpoints this)))])


(struct mutable-endpoint-submap-range-set abstract-mutable-range-set (delegate-range-set subrange)

  #:omit-define-syntaxes

  #:methods gen:range-set

  [(define/generic generic-range-set-comparator range-set-comparator)
   (define/generic generic-range-set-contains? range-set-contains?)
   (define/generic generic-range-set-encloses? range-set-encloses?)
   (define/generic generic-range-set-range-containing-or-absent range-set-range-containing-or-absent)

   (define (this-delegate-range-set this)
     (mutable-endpoint-submap-range-set-delegate-range-set this))

   (define (this-subrange this)
     (mutable-endpoint-submap-range-set-subrange this))

   (define (endpoints-submap this)
     (define delegate-endpoints
       (mutable-endpoint-map-range-set-endpoints (this-delegate-range-set this)))
     (endpoint-map-overlapping-submap delegate-endpoints (this-subrange this)))

   (define (in-range-set this #:descending? [descending? #false])
     (define cmp (range-set-comparator this))
     (define subendpoints (endpoints-submap this))
     (define size (sorted-map-size subendpoints))
     (for/stream ([endpoint-entry (in-sorted-map subendpoints #:descending? descending?)]
                  [i (in-naturals)])
       (match-define (entry lower upper) endpoint-entry)
       (define range (range-from-cuts lower upper #:comparator cmp))
       (if (or (zero? i) (equal? i (sub1 size)))
           (range-intersection range (this-subrange this))
           range)))

   (define (range-set-comparator this)
     (generic-range-set-comparator (this-delegate-range-set this)))

   (define (range-set-size this)
     (sorted-map-size (endpoints-submap this)))

   (define (range-set-contains? this value)
     (and (range-contains? (this-subrange this) value)
          (generic-range-set-contains? (this-delegate-range-set this) value)))

   (define (range-set-encloses? this range)
     (and (range-encloses? (this-subrange this) range)
          (generic-range-set-encloses? (this-delegate-range-set this) range)))

   (define (range-set-overlaps? this range)
     TODO)

   (define/guard (range-set-range-containing-or-absent this value)
     (define subrange (this-subrange this))
     (guard (range-contains? subrange value) #:else
       absent)
     (option-map (generic-range-set-range-containing-or-absent (this-delegate-range-set this) value)
                 (λ (r) (range-intersection subrange r))))

   (define (range-set-span-or-absent this)
     TODO)

   (define (range-subset this subset-range)
     (define new-range (range-intersection (this-subrange this) subset-range))
     (mutable-endpoint-submap-range-set (this-delegate-range-set this) new-range))]

  #:methods gen:mutable-range-set

  [(define/generic generic-range-set-add! range-set-add!)
   (define/generic generic-range-set-remove! range-set-remove!)

   (define (this-delegate-range-set this)
     (mutable-endpoint-submap-range-set-delegate-range-set this))

   (define (this-subrange this)
     (mutable-endpoint-submap-range-set-subrange this))

   (define/guard (range-set-add! this range)
     (define cmp (range-set-comparator (this-delegate-range-set this)))
     (check-precondition
      (equal? cmp (range-comparator range))
      (name range-set-add!)
      "added range does not use the same comparator as the range set"
      "range" range
      "range comparator" (range-comparator range)
      "range set comparator" cmp)
     (guard (not (empty-range? range)) #:else
       (void))
     (check-precondition
      (range-encloses? (this-subrange this) range)
      (name range-set-add!)
      "cannot add ranges outside the bounds of a range subset view"
      "range" range
      "subset boundary range" (this-subrange this))
     (generic-range-set-add! (this-delegate-range-set this) range))

   (define/guard (range-set-remove! this range)
     (define delegate (this-delegate-range-set this))
     (define cmp (range-set-comparator delegate))
     (check-precondition
      (equal? cmp (range-comparator range))
      (name range-set-remove!)
      "removed range does not use the same comparator as the range set"
      "range" range
      "range comparator" (range-comparator range)
      "range set comparator" cmp)
     (guard (not (empty-range? range)) #:else
       (void))
     (generic-range-set-remove! delegate (range-intersection (this-subrange this) range)))

   (define (range-set-clear! this)
     (generic-range-set-remove! (this-delegate-range-set this) (this-subrange this)))])


(define (make-mutable-range-set [initial-ranges '()] #:comparator comparator)
  (define ranges (sequence->vector initial-ranges))
  (check-ranges-use-comparator #:who (name make-mutable-range-set) ranges comparator)
  (define sorted-ranges (vector-sort ranges range<?))
  (check-ranges-disjoint #:who (name make-mutable-range-set) sorted-ranges)
  (define coalesced-ranges (vector-merge-adjacent sorted-ranges range-connected? range-span))
  (define endpoints (make-mutable-sorted-map #:key-comparator (cut<=> comparator)))
  (for ([range (in-vector coalesced-ranges)])
    (sorted-map-put! endpoints (range-lower-cut range) (range-upper-cut range)))
  (mutable-endpoint-map-range-set endpoints comparator))


(define (endpoint-map-contains? endpoints comparator value)
  (match (endpoint-map-get-nearest-range endpoints comparator (middle-cut value))
    [(== absent) #false]
    [(present nearest-range) (range-contains? nearest-range value)]))


(define (endpoint-map-encloses? endpoints comparator range)
  (match (endpoint-map-get-nearest-range endpoints comparator (range-lower-cut range))
    [(== absent) #false]
    [(present nearest-range) (range-encloses? nearest-range range)]))


(define/guard (endpoint-map-overlaps? endpoints comparator range)
  (define lower-cut (range-lower-cut range))
  (define upper-cut (range-upper-cut range))
  (guard-match (present (entry _ upper)) (sorted-map-entry-at-most endpoints upper-cut) #:else
    #false)
  (compare-infix (cut<=> (range-comparator range)) lower-cut < upper))


(define (endpoint-map-range-containing-or-absent endpoints comparator value)
  (option-filter
   (endpoint-map-get-nearest-range endpoints comparator (middle-cut value))
   (λ (nearest-range) (range-contains? nearest-range value))))


(define/guard (endpoint-map-span-or-absent endpoints comparator)
  (guard (not (sorted-map-empty? endpoints)) #:else
    absent)
  (match-define (present lower-cut) (sorted-map-least-key endpoints))
  (match-define (present (entry _ upper-cut)) (sorted-map-greatest-entry endpoints))
  (present (range-from-cuts lower-cut upper-cut #:comparator comparator)))


(define/guard (endpoint-map-get-nearest-range endpoints comparator cut)
  (guard-match (present (entry lower upper)) (sorted-map-entry-at-most endpoints cut) #:else
    absent)
  (present (range-from-cuts lower upper #:comparator comparator)))


(define (endpoint-map-overlapping-submap endpoints subrange)
  (define lower-cut (range-lower-cut subrange))
  (define upper-cut (range-upper-cut subrange))
  (define cmp (range-comparator subrange))
  (define true-lower-cut
    (match (sorted-map-entry-at-most endpoints lower-cut)
      [(present (entry nearest-smaller-range-lower-cut nearest-smaller-range-upper-cut))
       (define nearest-smaller-range
         (range-from-cuts nearest-smaller-range-lower-cut nearest-smaller-range-upper-cut
                          #:comparator cmp))
       (if (range-overlaps? nearest-smaller-range subrange)
           nearest-smaller-range-lower-cut
           lower-cut)]
      [(== absent) lower-cut]))
  (define endpoint-subrange (closed-open-range true-lower-cut upper-cut #:comparator (cut<=> cmp)))
  (sorted-submap endpoints endpoint-subrange))


;@----------------------------------------------------------------------------------------------------
;; Construction APIs


(define (range-set #:comparator [comparator #false] . ranges)
  (check-precondition
   (or comparator (not (empty? ranges)))
   (name range-set)
   "cannot construct an empty range set without a comparator")
  (let ([comparator (or comparator (range-comparator (first ranges)))])
    (sequence->range-set ranges #:comparator comparator)))


(define (sequence->range-set ranges #:comparator comparator)
  (transduce ranges #:into (into-range-set comparator)))


(struct range-set-builder ([range-vector-builder #:mutable] comparator))


(define (make-range-set-builder comparator)
  (range-set-builder (make-vector-builder) comparator))


(define (range-set-builder-add builder range)
  (define vector-builder (vector-builder-add (range-set-builder-range-vector-builder builder) range))
  (set-range-set-builder-range-vector-builder! builder vector-builder)
  builder)


(define (build-range-set builder)
  (define ranges (build-vector (range-set-builder-range-vector-builder builder)))
  (define comparator (range-set-builder-comparator builder))
  (check-ranges-use-comparator #:who (name build-range-set) ranges comparator)
  (define sorted-ranges (vector-sort ranges range<?))
  (check-ranges-disjoint #:who (name build-range-set) sorted-ranges)
  (define coalesced-ranges (vector-merge-adjacent sorted-ranges range-connected? range-span))
  (define endpoints
    (for/sorted-map #:key-comparator (cut<=> comparator) ([range (in-vector coalesced-ranges)])
      (entry (range-lower-cut range) (range-upper-cut range))))
  (immutable-endpoint-map-range-set endpoints comparator))


(define (check-ranges-use-comparator #:who who ranges comparator)
  (for ([range (in-vector ranges)])
    (check-precondition
     (equal? (range-comparator range) comparator)
     who
     "not all ranges use the same comparator"
     "range" range
     "range comparator" (range-comparator range)
     "expected comparator" comparator)))


(define (range<? range other-range)
  (equal? (compare range<=> range other-range) lesser))


(define (check-ranges-disjoint #:who who ranges)
  (unless (zero? (vector-length ranges))
    (for ([range (in-vector ranges)]
          [next-range (in-vector ranges 1)])
      (when (range-overlaps? range next-range)
        (raise-arguments-error
         who
         "overlapping ranges not allowed"
         "range" range
         "next range" next-range)))))


(define (into-range-set comparator)

  (define (start)
    (make-range-set-builder comparator))
  
  (make-effectful-fold-reducer
   range-set-builder-add start build-range-set #:name (name into-range-set)))


(define-syntax-parse-rule (for/range-set #:comparator comparator clauses body)
  #:declare comparator (expr/c #'comparator?)
  #:declare body (for-body this-syntax)
  #:with context this-syntax
  (for/reducer/derived context (into-range-set comparator.c) clauses (~@ . body)))


(define-syntax-parse-rule (for*/range-set #:comparator comparator clauses body)
  #:declare comparator (expr/c #'comparator?)
  #:declare body (for-body this-syntax)
  #:with context this-syntax
  (for*/reducer/derived context (into-range-set comparator.c) clauses (~@ . body)))
