#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sorted-map? (-> any/c boolean?)]
  [immutable-sorted-map? (-> any/c boolean?)]
  [mutable-sorted-map? (-> any/c boolean?)]
  [in-sorted-map (->* (sorted-map?) (#:descending? boolean?) (sequence/c entry?))]
  [in-sorted-map-keys (->* (sorted-map?) (#:descending? boolean?) (sequence/c any/c))]
  [in-sorted-map-values (->* (sorted-map?) (#:descending? boolean?) (sequence/c any/c))]
  [sorted-map-empty? (-> sorted-map? boolean?)]
  [sorted-map-size (-> sorted-map? exact-nonnegative-integer?)]
  [sorted-map-key-comparator (-> sorted-map? comparator?)]
  [sorted-map-contains-key? (-> sorted-map? any/c boolean?)]
  [sorted-map-contains-value? (-> sorted-map? any/c boolean?)]
  [sorted-map-contains-entry? (-> sorted-map? entry? boolean?)]
  [sorted-map-least-key (-> sorted-map? option?)]
  [sorted-map-greatest-key (-> sorted-map? option?)]
  [sorted-map-key-less-than (-> sorted-map? any/c option?)]
  [sorted-map-key-at-most (-> sorted-map? any/c option?)]
  [sorted-map-key-greater-than (-> sorted-map? any/c option?)]
  [sorted-map-key-at-least (-> sorted-map? any/c option?)]
  [sorted-map-least-entry (-> sorted-map? (option/c entry?))]
  [sorted-map-greatest-entry (-> sorted-map? (option/c entry?))]
  [sorted-map-entry-less-than (-> sorted-map? any/c (option/c entry?))]
  [sorted-map-entry-at-most (-> sorted-map? any/c (option/c entry?))]
  [sorted-map-entry-greater-than (-> sorted-map? any/c (option/c entry?))]
  [sorted-map-entry-at-least (-> sorted-map? any/c (option/c entry?))]
  [sorted-map-get (->* (sorted-map? any/c) (failure-result/c) any/c)]
  [sorted-map-get! (-> mutable-sorted-map? any/c failure-result/c any/c)]
  [sorted-map-get-option (-> sorted-map? any/c option?)]
  [sorted-map-get-entry (->* (sorted-map? any/c) (failure-result/c) entry?)]
  [sorted-map-get-entry! (-> mutable-sorted-map? any/c failure-result/c entry?)]
  [sorted-map-put (-> immutable-sorted-map? any/c any/c immutable-sorted-map?)]
  [sorted-map-put! (-> mutable-sorted-map? any/c any/c void?)]
  [sorted-map-put-all (-> immutable-sorted-map? (sequence/c entry?) immutable-sorted-map?)]
  [sorted-map-put-all! (-> mutable-sorted-map? (sequence/c entry?) void?)]
  [sorted-map-put-if-absent
   (-> immutable-sorted-map? any/c any/c (result/c immutable-sorted-map? any/c))]
  [sorted-map-put-if-absent! (-> mutable-sorted-map? any/c any/c option?)]
  [sorted-map-update
   (->* (immutable-sorted-map? any/c (-> any/c any/c)) (failure-result/c) immutable-sorted-map?)]
  [sorted-map-update! (->* (mutable-sorted-map? any/c (-> any/c any/c)) (failure-result/c) void?)]
  [sorted-map-remove (-> immutable-sorted-map? any/c immutable-sorted-map?)]
  [sorted-map-remove! (-> mutable-sorted-map? any/c void?)]
  [sorted-map-remove-all (-> immutable-sorted-map? (sequence/c any/c) immutable-sorted-map?)]
  [sorted-map-remove-all! (-> mutable-sorted-map? (sequence/c any/c) void?)]
  [sorted-map-clear! (-> mutable-sorted-map? void?)]
  [sorted-map-keys (-> sorted-map? sorted-set?)]
  [sorted-map-entries (-> sorted-map? sorted-set?)]
  [sorted-submap (-> sorted-map? range? sorted-map?)]
  [sorted-map-reverse (-> sorted-map? sorted-map?)]))


;; The APIs for creating the generic, extensible hierarchy of collection implementations exist only to
;; make it easier to organize Rebellion's various implementations. They are *not* designed for
;; external consumption, and no promises of API stability or quality are made. Please do not make your
;; own implementations of these interfaces; instead file an issue at
;; https://github.com/jackfirth/rebellion/issues describing your use case. These APIs might be made
;; public in the future, depending on feedback from users.
(module+ private-for-rebellion-only
  (provide
   (struct-out abstract-sorted-map)
   (struct-out abstract-mutable-sorted-map)
   (struct-out abstract-immutable-sorted-map)
   gen:sorted-map
   gen:mutable-sorted-map
   gen:immutable-sorted-map
   (contract-out
    [default-sorted-map-lookup-failure-result
      (-> interned-symbol? any/c sorted-map? failure-result/c)])))


(require guard
         racket/generic
         racket/match
         racket/sequence
         racket/stream
         racket/unsafe/ops
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/base/result
         rebellion/base/symbol
         rebellion/collection/entry
         rebellion/collection/private/sorted-set-interface
         rebellion/private/printer-markup)


;@----------------------------------------------------------------------------------------------------


;; This is the API for *unmodifiable* sorted maps. Unmodifiable sorted maps do not expose an API for
;; clients to mutate them, but they make no guarantees that they will not mutate of their own accord.
;; For example, one module may wish to provide an *unmodifiable view* of a mutable sorted map to
;; clients to prevent uncontrolled external mutation. Such a view is unmodifiable, but not immutable,
;; as the underlying map backing the view may mutate.
(define-generics sorted-map

  ;; descending? should always default to false
  (in-sorted-map sorted-map #:descending? [descending?])
  (in-sorted-map-keys sorted-map #:descending? [descending?])
  (in-sorted-map-values sorted-map #:descending? [descending?])
  
  (sorted-map-empty? sorted-map)
  (sorted-map-size sorted-map)
  (sorted-map-key-comparator sorted-map)
  (sorted-map-contains-key? sorted-map key)
  (sorted-map-contains-value? sorted-map value)
  (sorted-map-contains-entry? sorted-map entry)
  (sorted-map-get sorted-map key [failure-result])
  (sorted-map-get-option sorted-map key)
  (sorted-map-get-entry sorted-map key [failure-result])
  (sorted-map-least-key sorted-map)
  (sorted-map-greatest-key sorted-map)
  (sorted-map-key-less-than sorted-map upper-bound)
  (sorted-map-key-at-most sorted-map upper-bound)
  (sorted-map-key-greater-than sorted-map lower-bound)
  (sorted-map-key-at-least sorted-map lower-bound)
  (sorted-map-least-entry sorted-map)
  (sorted-map-greatest-entry sorted-map)
  (sorted-map-entry-less-than sorted-map upper-key-bound)
  (sorted-map-entry-at-most sorted-map upper-key-bound)
  (sorted-map-entry-greater-than sorted-map lower-key-bound)
  (sorted-map-entry-at-least sorted-map lower-key-bound)
  (sorted-map-keys sorted-map)
  (sorted-map-entries sorted-map)
  (sorted-submap sorted-map key-range)
  (sorted-map-reverse sorted-map)

  #:fallbacks

  [(define/generic generic-sorted-map-size sorted-map-size)
   (define/generic generic-in-sorted-map in-sorted-map)
   (define/generic generic-sorted-map-get-option sorted-map-get-option)

   (define (in-sorted-map-keys this #:descending? [descending? #false])
     (for/stream ([e (generic-in-sorted-map this #:descending? descending?)])
       (entry-key e)))

   (define (in-sorted-map-values this #:descending? [descending? #false])
     (for/stream ([e (generic-in-sorted-map this #:descending? descending?)])
       (entry-value e)))
   
   (define (sorted-map-empty? this)
     (zero? (generic-sorted-map-size this)))

   (define (sorted-map-contains-entry? this e)
     (match (generic-sorted-map-get-option this (entry-key e))
       [(== absent) #false]
       [(present actual-value) (equal? actual-value (entry-value e))]))])


;; Subtypes must implement the gen:sorted-map interface.
(struct abstract-sorted-map ()

  #:property prop:sequence in-sorted-map

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer-with-markup
      'sorted-map
      (λ (this)
        (for/list ([e (in-sorted-map this)])
          (sequence-markup (list (entry-key e) (entry-value e)))))))])


(define-generics mutable-sorted-map

  (sorted-map-get! mutable-sorted-map key failure-result)
  (sorted-map-get-entry! mutable-sorted-map key failure-result)
  (sorted-map-put! mutable-sorted-map key value)
  (sorted-map-put-all! mutable-sorted-map entries)
  (sorted-map-put-if-absent! mutable-sorted-map key value)
  (sorted-map-update! mutable-sorted-map key updater [failure-result])
  (sorted-map-remove! mutable-sorted-map key)
  (sorted-map-remove-all! mutable-sorted-map keys)
  (sorted-map-clear! mutable-sorted-map)

  #:fallbacks

  [(define/generic generic-sorted-map-remove! sorted-map-remove!)

   (define (sorted-map-remove-all! this keys)
     (for ([key keys])
       (generic-sorted-map-remove! this key)))])


;; Subtypes must implement the gen:sorted-map interface *and* the gen:mutable-sorted-map interface.
;; Mutable sorted maps don't implement gen:equal+hash because two mutable objects should only be equal
;; if they have the same identity: that is, if mutations to one are reflected by the other. This does
;; not necessarily mean only eq? mutable objects should be equal?, as it's perfectly fine for a
;; wrapper or view of a mutable object to be equal? to that object.
(struct abstract-mutable-sorted-map abstract-sorted-map ()

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer-with-markup
      'mutable-sorted-map
      (λ (this)
        (for/list ([e (in-sorted-map this)])
          (sequence-markup (list (entry-key e) (entry-value e)))))))])


(define-generics immutable-sorted-map

  (sorted-map-put immutable-sorted-map key value)
  (sorted-map-put-all immutable-sorted-map entries)
  (sorted-map-put-if-absent immutable-sorted-map key value)
  (sorted-map-update immutable-sorted-map key updater [failure-result])
  (sorted-map-remove immutable-sorted-map key)
  (sorted-map-remove-all immutable-sorted-map keys)

  #:fallbacks

  [(define/generic generic-sorted-map-remove sorted-map-remove)

   (define (sorted-map-remove-all this keys)
     (for/fold ([this this])
               ([k keys])
       (generic-sorted-map-remove this k)))])


;; Subtypes must implement the gen:sorted-map interface *and* the gen:immutable-sorted-map interface.
(struct abstract-immutable-sorted-map abstract-sorted-map ()

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer-with-markup
      'immutable-sorted-map
      (λ (this)
        (for/list ([e (in-sorted-map this)])
          (sequence-markup (list (entry-key e) (entry-value e)))))))]

  ;; Immutable sorted maps are always compared structurally. Two immutable sorted maps are equal if
  ;; they use equal comparators and they contain the same entries.
  #:methods gen:equal+hash

  [(define/guard (equal-proc this other recur)

     (guard (recur (sorted-map-key-comparator this) (sorted-map-key-comparator other)) #:else
       #false)

     ;; We check emptiness as a fast path, since empty collections are common in practice and
     ;; easy to optimize for.
     (guard (not (sorted-map-empty? this)) #:else
       (sorted-map-empty? other))
     (guard (not (sorted-map-empty? other)) #:else
       #false)

     ;; We check the size before comparing elements so that we can avoid paying the O(n) element
     ;; comparison cost most of the time.
     (and (recur (sorted-map-size this) (sorted-map-size other))
          (for/and ([this-entry (in-sorted-map this)]
                    [other-entry (in-sorted-map other)])
            (recur this-entry other-entry))))

   (define (hash-proc this recur)
     (for/fold ([hash-code (recur (sorted-map-key-comparator this))])
               ([entry (in-sorted-map this)])
       (unsafe-fx+/wraparound hash-code (recur entry))))

   (define hash2-proc hash-proc)])


(define ((default-sorted-map-lookup-failure-result name key map))
  (define message
    (format "~a: no value for key;\n  key: ~e\n  map: ~e" name key map))
  (raise (make-exn:fail:contract message (current-continuation-marks))))
