#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [range-set? predicate/c]
  [immutable-range-set? predicate/c]
  [mutable-range-set? predicate/c]
  [in-range-set (->* (range-set?) (#:descending? boolean?) (sequence/c nonempty-range?))]
  [range-set-comparator (-> range-set? comparator?)]
  [range-set-empty? (-> range-set? boolean?)]
  [range-set-size (-> range-set? exact-nonnegative-integer?)]
  [range-set-contains? (-> range-set? any/c boolean?)]
  [range-set-contains-all? (-> range-set? (sequence/c any/c) boolean?)]
  [range-set-encloses? (-> range-set? range? boolean?)]
  [range-set-encloses-all? (-> range-set? (sequence/c range?) boolean?)]
  [range-set-intersects? (-> range-set? range? boolean?)]
  [range-set-range-containing (->* (range-set? any/c) (failure-result/c) any)]
  [range-set-range-containing-or-absent (-> range-set? any/c (option/c range?))]
  [range-set-span (->* (range-set?) (failure-result/c) any)]
  [range-set-span-or-absent (-> range-set? (option/c range?))]
  [range-set-add (-> immutable-range-set? range? immutable-range-set?)]
  [range-set-add! (-> mutable-range-set? range? void?)]
  [range-set-add-all (-> immutable-range-set? (sequence/c range?) immutable-range-set?)]
  [range-set-add-all! (-> mutable-range-set? (sequence/c range?) void?)]
  [range-set-remove (-> immutable-range-set? range? immutable-range-set?)]
  [range-set-remove! (-> mutable-range-set? range? void?)]
  [range-set-remove-all (-> immutable-range-set? (sequence/c range?) immutable-range-set?)]
  [range-set-remove-all! (-> mutable-range-set? (sequence/c range?) void?)]
  [range-set-clear! (-> mutable-range-set? void?)]
  [range-subset (-> range-set? range? range-set?)]))


;; The APIs for creating the generic, extensible hierarchy of collection implementations exist only to
;; make it easier to organize Rebellion's various implementations. They are *not* designed for
;; external consumption, and no promises of API stability or quality are made. Please do not make your
;; own implementations of these interfaces; instead file an issue at
;; https://github.com/jackfirth/rebellion/issues describing your use case. These APIs might be made
;; public in the future, depending on feedback from users.
(module+ private-for-rebellion-only
  (provide
   (struct-out abstract-range-set)
   (struct-out abstract-mutable-range-set)
   (struct-out abstract-immutable-range-set)
   gen:range-set
   gen:mutable-range-set
   gen:immutable-range-set
   (contract-out
    [default-range-set-value-not-contained-failure-result (-> any/c range-set? failure-result/c)]
    [default-empty-range-set-span-failure-result failure-result/c])))


(require racket/generic
         racket/match
         racket/sequence
         racket/unsafe/ops
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/private/guarded-block
         rebellion/private/printer-markup
         rebellion/private/static-name)


;@----------------------------------------------------------------------------------------------------


;; This is the API for *unmodifiable* range sets. Unmodifiable range sets do not expose an API for
;; clients to mutate them, but they make no guarantees that they will not mutate of their own accord.
;; For example, one module may wish to provide an *unmodifiable view* of a mutable range set to
;; clients to prevent uncontrolled external mutation. Such a view is unmodifiable, but not immutable,
;; as the underlying map backing the view may mutate.
(define-generics range-set

  ;; descending? should always default to false
  (in-range-set range-set #:descending? [descending?])

  (range-set-comparator range-set)
  (range-set-empty? range-set)
  (range-set-size range-set)
  (range-set-contains? range-set value)
  (range-set-contains-all? range-set values)
  (range-set-encloses? range-set range)
  (range-set-encloses-all? range-set ranges)
  (range-set-intersects? range-set range)
  (range-set-range-containing range-set value [failure-result])
  (range-set-range-containing-or-absent range-set value)
  (range-set-span range-set [failure-result])
  (range-set-span-or-absent range-set)
  (range-subset range-set subrange)

  #:fallbacks

  [(define/generic generic-size range-set-size)
   (define/generic generic-contains? range-set-contains?)
   (define/generic generic-encloses? range-set-encloses?)
   (define/generic generic-range-containing-or-absent range-set-range-containing-or-absent)
   (define/generic generic-span-or-absent range-set-span-or-absent)
   
   (define (range-set-empty? this)
     (zero? (generic-size this)))

   (define (range-set-contains-all? this values)
     (for/and ([value values])
       (generic-contains? this value)))

   (define (range-set-encloses-all? this ranges)
     (for/and ([range ranges])
       (generic-encloses? this range)))

   (define
     (range-set-range-containing
      this
      value
      [failure-result (default-range-set-value-not-contained-failure-result this value)])
     (match (generic-range-containing-or-absent this value)
       [(present range) range]
       [(== absent) (if (procedure? failure-result) (failure-result) failure-result)]))

   (define (range-set-span this [failure-result default-empty-range-set-span-failure-result])
     (match (generic-span-or-absent this)
       [(present span) span]
       [(== absent) (if (procedure? failure-result) (failure-result) failure-result)]))])


(define ((default-range-set-value-not-contained-failure-result range-set value))
  (define message
    (format "~a: no range containing value;\n  ranges: ~e  value: ~e\n"
            (name range-set-range-containing) range-set value))
  (raise (make-exn:fail:contract message (current-continuation-marks))))


(define (default-empty-range-set-span-failure-result)
  (define message
    (format "~a: range set is empty and has no span" (name range-set-span)))
  (raise (make-exn:fail:contract message (current-continuation-marks))))


;; Subtypes must implement the gen:range-set interface.
(struct abstract-range-set ()

  #:property prop:sequence in-range-set

  #:methods gen:custom-write

  [(define write-proc (make-constructor-style-printer-with-markup 'range-set in-range-set))])


;@----------------------------------------------------------------------------------------------------


(define-generics immutable-range-set

  (range-set-add immutable-range-set range)
  (range-set-add-all immutable-range-set ranges)
  (range-set-remove immutable-range-set range)
  (range-set-remove-all immutable-range-set ranges)

  #:fallbacks

  [(define/generic generic-add range-set-add)
   (define/generic generic-remove range-set-remove)

   (define (range-set-add-all this ranges)
     (for/fold ([this this])
               ([range ranges])
       (generic-add this range)))

   (define (range-set-remove-all this ranges)
     (for/fold ([this this])
               ([range ranges])
       (generic-remove this range)))])


;; Subtypes must implement the gen:range-set interface *and* the gen:immutable-range-set interface.
(struct abstract-immutable-range-set abstract-range-set ()

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer-with-markup 'immutable-range-set in-range-set))]

  ;; Immutable range sets are always compared structurally. Two immutable range sets are equal if
  ;; they use equal comparators and they contain the same ranges.
  #:methods gen:equal+hash

  [(define/guard (equal-proc this other recur)

     (guard (recur (range-set-comparator this) (range-set-comparator other)) else
       #false)

     ;; We check emptiness as a fast path, since empty collections are common in practice and
     ;; easy to optimize for.
     (guard (range-set-empty? this) then
       (range-set-empty? other))
     (guard (range-set-empty? other) then
       #false)

     ;; We check the size before comparing elements so that we can avoid paying the O(n) range
     ;; comparison cost most of the time.
     (and (recur (range-set-size this) (range-set-size other))
          (for/and ([this-range (in-range-set this)]
                    [other-range (in-range-set other)])
            (recur this-range other-range))))

   (define (hash-proc this recur)
     (for/fold ([hash-code (recur (range-set-comparator this))])
               ([range (in-range-set this)])
       (unsafe-fx+/wraparound hash-code (recur range))))

   (define hash2-proc hash-proc)])


;@----------------------------------------------------------------------------------------------------


(define-generics mutable-range-set

  (range-set-add! mutable-range-set range)
  (range-set-add-all! mutable-range-set ranges)
  (range-set-remove! mutable-range-set range)
  (range-set-remove-all! mutable-range-set ranges)
  (range-set-clear! mutable-range-set)

  #:fallbacks

  [(define/generic generic-add! range-set-add!)
   (define/generic generic-remove! range-set-remove!)

   (define (range-set-add-all! this ranges)
     (for ([range ranges])
       (generic-add! this range)))

   (define (range-set-remove-all! this ranges)
     (for ([range ranges])
       (generic-remove! this range)))])


;; Subtypes must implement the gen:range-set interface *and* the gen:mutable-range-set interface.
;; Mutable range sets don't implement gen:equal+hash because two mutable objects should only be equal
;; if they have the same identity: that is, if mutations to one are reflected by the other. This does
;; not necessarily mean only eq? mutable objects should be equal?, as it's perfectly fine for a
;; wrapper or view of a mutable object to be equal? to that object.
(struct abstract-mutable-range-set abstract-range-set ()

  #:methods gen:custom-write

  [(define write-proc (make-constructor-style-printer-with-markup 'mutable-range-set in-range-set))])
