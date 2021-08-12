#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-set? predicate/c]
  [immutable-sorted-set? predicate/c]
  [mutable-sorted-set? predicate/c]
  [in-sorted-set (->* (sorted-set?) (#:descending? boolean?) (sequence/c any/c))]
  [sorted-set-size (-> sorted-set? exact-nonnegative-integer?)]
  [sorted-set-comparator (-> sorted-set? comparator?)]
  [sorted-set-contains? (-> sorted-set? any/c boolean?)]
  [sorted-set-contains-all? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-contains-any? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-contains-none? (-> sorted-set? (sequence/c any/c) boolean?)]
  [sorted-set-least-element (-> sorted-set? option?)]
  [sorted-set-greatest-element (-> sorted-set? option?)]
  [sorted-set-element-less-than (-> sorted-set? any/c option?)]
  [sorted-set-element-at-most (-> sorted-set? any/c option?)]
  [sorted-set-element-greater-than (-> sorted-set? any/c option?)]
  [sorted-set-element-at-least (-> sorted-set? any/c option?)]
  [sorted-set-add (-> immutable-sorted-set? any/c immutable-sorted-set?)]
  [sorted-set-add! (-> mutable-sorted-set? any/c void?)]
  [sorted-set-add-all (-> immutable-sorted-set? (sequence/c any/c) immutable-sorted-set?)]
  [sorted-set-add-all! (-> mutable-sorted-set? (sequence/c any/c) void?)]
  [sorted-set-remove (-> immutable-sorted-set? any/c immutable-sorted-set?)]
  [sorted-set-remove! (-> mutable-sorted-set? any/c void?)]
  [sorted-set-remove-all (-> immutable-sorted-set? (sequence/c any/c) immutable-sorted-set?)]
  [sorted-set-remove-all! (-> mutable-sorted-set? (sequence/c any/c) void?)]
  [sorted-set-clear! (-> mutable-sorted-set? void?)]
  [sorted-subset (-> sorted-set? range? sorted-set?)]
  [sorted-set-reverse (-> sorted-set? sorted-set?)]))


;; The APIs for creating the generic, extensible hierarchy of collection implementations exist only to
;; make it easier to organize Rebellion's various implementations. They are *not* designed for
;; external consumption, and no promises of API stability or quality are made. Please do not make your
;; own implementations of these interfaces; instead file an issue at
;; https://github.com/jackfirth/rebellion/issues describing your use case. These APIs might be made
;; public in the future, depending on feedback from users.
(module+ private-for-rebellion-only
  (provide
   (struct-out abstract-sorted-set)
   (struct-out abstract-mutable-sorted-set)
   (struct-out abstract-immutable-sorted-set)
   gen:sorted-set
   gen:mutable-sorted-set
   gen:immutable-sorted-set))


(require racket/generic
         racket/sequence
         racket/struct
         racket/unsafe/ops
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/private/guarded-block)


;@----------------------------------------------------------------------------------------------------


;; This is the API for *unmodifiable* sorted sets. Unmodifiable sorted sets do not expose an API for
;; clients to mutate them, but they make no guarantees that they will not mutate of their own accord.
;; For example, one module may wish to provide an *unmodifiable view* of a mutable sorted set to
;; clients to prevent uncontrolled external mutation. Such a view is unmodifiable, but not immutable,
;; as the underlying set backing the view may mutate.
(define-generics sorted-set

  (in-sorted-set sorted-set #:descending? [descending?]) ;; descending? should always default to false
  (sorted-set-empty? sorted-set)
  (sorted-set-size sorted-set)
  (sorted-set-comparator sorted-set)
  (sorted-set-contains? sorted-set value)
  (sorted-set-contains-any? sorted-set values)
  (sorted-set-contains-all? sorted-set values)
  (sorted-set-contains-none? sorted-set values)
  (sorted-set-least-element sorted-set)
  (sorted-set-greatest-element sorted-set)
  (sorted-set-element-less-than sorted-set upper-bound)
  (sorted-set-element-greater-than sorted-set lower-bound)
  (sorted-set-element-at-most sorted-set upper-bound)
  (sorted-set-element-at-least sorted-set lower-bound)
  (sorted-subset sorted-set element-range)
  (sorted-set-reverse sorted-set)

  #:fallbacks

  [(define/generic generic-sorted-set-size sorted-set-size)
   (define/generic generic-sorted-set-contains? sorted-set-contains?)
   (define/generic generic-sorted-set-contains-any? sorted-set-contains-any?)

   (define (sorted-set-empty? this)
     (equal? (generic-sorted-set-size this) 0))

   (define (sorted-set-contains-any? this values)
     (for/or ([v values])
       (generic-sorted-set-contains? this v)))

   (define (sorted-set-contains-all? this values)
     (for/and ([v values])
       (generic-sorted-set-contains? this v)))

   (define (sorted-set-contains-none? this values)
     (not (generic-sorted-set-contains-any? this values)))])


;; Subtypes must implement the gen:sorted-set interface.
(struct abstract-sorted-set ()

  #:property prop:sequence in-sorted-set

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer
      (λ (this) 'sorted-set)
      (λ (this) (in-sorted-set this))))])


(define-generics mutable-sorted-set

  (sorted-set-add! mutable-sorted-set element)
  (sorted-set-add-all! mutable-sorted-set elements)
  (sorted-set-remove! mutable-sorted-set element)
  (sorted-set-remove-all! mutable-sorted-set elements)
  (sorted-set-clear! mutable-sorted-set)

  #:fallbacks

  [(define/generic generic-sorted-set-add! sorted-set-add!)
   (define/generic generic-sorted-set-remove! sorted-set-remove!)

   (define (sorted-set-add-all! this elements)
     (for ([element elements])
       (generic-sorted-set-add! this element)))

   (define (sorted-set-remove-all! this elements)
     (for ([element elements])
       (generic-sorted-set-add! this element)))])


;; Subtypes must implement the gen:sorted-set interface *and* the gen:mutable-sorted-set interface.
;; Mutable sorted sets don't implement gen:equal+hash because two mutable objects should only be equal
;; if they have the same identity: that is, if mutations to one are reflected by the other. This does
;; not necessarily mean only eq? mutable objects should be equal?, as it's perfectly fine for a
;; wrapper or view of a mutable object to be equal? to that object.
(struct abstract-mutable-sorted-set abstract-sorted-set ()

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer
      (λ (this) 'mutable-sorted-set)
      (λ (this) (in-sorted-set this))))])


(define-generics immutable-sorted-set

  (sorted-set-add immutable-sorted-set element)
  (sorted-set-add-all immutable-sorted-set elements)
  (sorted-set-remove immutable-sorted-set element)
  (sorted-set-remove-all immutable-sorted-set elements)

  #:fallbacks

  [(define/generic generic-sorted-set-add sorted-set-add)
   (define/generic generic-sorted-set-remove sorted-set-remove)

   (define (sorted-set-add-all this elements)
     (for/fold ([this this])
               ([element elements])
       (generic-sorted-set-add this element)))

   (define (sorted-set-remove-all this elements)
     (for/fold ([this this])
               ([element elements])
       (generic-sorted-set-remove this element)))])


;; Subtypes must implement the gen:sorted-set interface *and* the gen:immutable-sorted-set interface.
(struct abstract-immutable-sorted-set abstract-sorted-set ()

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer
      (λ (this) 'immutable-sorted-set)
      (λ (this) (in-sorted-set this))))]

  ;; Immutable sorted sets are always compared structurally. Two immutable sorted sets are equal if
  ;; they use equal comparators and they contain equal elements.
  #:methods gen:equal+hash

  [(define/guard (equal-proc this other recur)

     ;; We check emptiness first as a fast path, since empty collections are common in practice and
     ;; easy to optimize for.
     (guard (sorted-set-empty? this) then
       (sorted-set-empty? other))
     (guard (sorted-set-empty? other) then
       #false)

     ;; We check the size before comparing elements so that we can avoid paying the O(n) element
     ;; comparison cost most of the time.
     (and (recur (sorted-set-size this) (sorted-set-size other))
          (recur (sorted-set-comparator this) (sorted-set-comparator other))
          (for/and ([this-element (in-sorted-set this)]
                    [other-element (in-sorted-set other)])
            (recur this-element other-element))))

   (define (hash-proc this recur)
     (for/fold ([hash-code (unsafe-fx+/wraparound (recur (sorted-set-comparator this)))])
               ([element (in-sorted-set this)])
       (unsafe-fx+/wraparound hash-code (recur element))))

   (define hash2-proc hash-proc)])
