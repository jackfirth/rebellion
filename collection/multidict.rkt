#lang racket/base

(require racket/contract/base)

(provide
 for/multidict
 for*/multidict
 (contract-out
  [multidict (->* () #:rest key-value-list/c multidict?)]
  [multidict? predicate/c]
  [multidict-add (-> multidict? any/c any/c multidict?)]
  [multidict-add-entry (-> multidict? entry? multidict?)]
  [multidict-remove (-> multidict? any/c any/c multidict?)]
  [multidict-remove-entry (-> multidict? entry? multidict?)]
  [multidict-replace-values (-> multidict? any/c set-coercible/c multidict?)]
  [multidict-size (-> multidict? natural?)]
  [multidict-ref (-> multidict? any/c set?)]
  [multidict-keys (-> multidict? multiset?)]
  [multidict-values (-> multidict? multiset?)]
  [multidict-unique-keys (-> multidict? set?)]
  [multidict-unique-values (-> multidict? set?)]
  [multidict-entries (-> multidict? (set/c entry? #:cmp 'equal))]
  [multidict-contains-key? (-> multidict? any/c boolean?)]
  [multidict-contains-value? (-> multidict? any/c boolean?)]
  [multidict-contains-entry? (-> multidict? entry? boolean?)]
  [multidict->hash
   (-> multidict?
       (hash/c any/c nonempty-set? #:immutable #t #:flat? #t))]
  [multidict-inverse (-> multidict? multidict?)]
  [empty-multidict empty-multidict?]
  [empty-multidict? predicate/c]
  [nonempty-multidict? predicate/c]
  [in-multidict-entries (-> multidict? (sequence/c entry?))]
  [into-multidict (reducer/c entry? multidict?)]))

(require (for-syntax racket/base)
         racket/list
         racket/math
         racket/sequence
         racket/set
         racket/stream
         rebellion/collection/entry
         rebellion/collection/multiset
         rebellion/collection/keyset
         rebellion/private/guarded-block
         rebellion/private/printer-markup
         rebellion/streaming/reducer
         rebellion/type/record)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define key-value-list/c
  (or/c null?
        (cons/c any/c
                (cons/c any/c
                        (recursive-contract key-value-list/c #:flat)))))

(define (nonempty-set? v)
  (and (set? v) (not (zero? (set-count v)))))

(define set-coercible/c
  (or/c set? multiset? (sequence/c any/c)))

(define/guard (sequence->set seq)
  (guard (set? seq) then seq)
  (guard (multiset? seq) then (multiset-unique-elements seq))
  (for/set ([v seq]) v))

(define (make-multidict-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define backing-hash-field
    (keyset-index-of (record-type-fields type) '#:backing-hash))
  (define accessor (record-descriptor-accessor descriptor))
  (define equal+hash (default-record-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     type-name
     (λ (this)
       (for*/list ([e (sequence this)])
         (sequence-markup (list (entry-key e) (entry-value e)))))))
  (define (sequence this)
    (define backing-hash (accessor this backing-hash-field))
    (for*/stream ([(k vs) (in-immutable-hash backing-hash)]
                  [v (in-immutable-set vs)])
      (entry k v)))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:sequence sequence)
        (cons prop:custom-write custom-write)))

(define-record-type set-delta (extra-elements missing-elements))

(define (set-compute-differences set other-set)
  (set-delta #:extra-elements (set-subtract set other-set)
             #:missing-elements (set-subtract other-set set)))

(define (set-delta-size-change delta)
  (define extra (set-delta-extra-elements delta))
  (define missing (set-delta-missing-elements delta))
  (- (set-count missing) (set-count extra)))

;@------------------------------------------------------------------------------
(define-record-type multidict (size backing-hash inverted-hash keys values)
  #:omit-root-binding
  #:property-maker make-multidict-properties)

(define (in-multidict-entries dict) dict)

(define empty-multidict
  (constructor:multidict #:size 0
                         #:backing-hash (hash)
                         #:inverted-hash (hash)
                         #:keys empty-multiset
                         #:values empty-multiset))

(define (multidict-replace-values
         dict k seq
         ;; The precomputed delta argument is used by the multidict-add and
         ;; multidict-remove functions to avoid possible O(n) costs in computing
         ;; the delta. Both of those functions already know the delta: it will
         ;; either be empty, a single extra element, or a single missing
         ;; element. If we didn't precompute, then repeatedly calling add or
         ;; remove on a multidict with the same key would result in quadratic
         ;; performance.
         #:precomputed-value-set-delta [precomputed-delta #f])
  (define vs (sequence->set seq))
  (define old-vs (multidict-ref dict k))
  (define delta (or precomputed-delta (set-compute-differences old-vs vs)))
  (define size-delta (set-delta-size-change delta))
  (define size (+ (multidict-size dict) size-delta))
  (define old-backing-hash (multidict-backing-hash dict))
  (define backing-hash
    (if (set-empty? vs)
        (hash-remove old-backing-hash k)
        (hash-set old-backing-hash k vs)))
  (define inverted-hash
    (update-inverted-hash-from-value-mappings-delta
     (multidict-inverted-hash dict)
     k
     delta))
  (define old-keys (multidict-keys dict))
  (define new-key-freq (+ (multiset-frequency old-keys k) size-delta))
  (define keys (multiset-set-frequency old-keys k new-key-freq))
  (define old-all-values (multidict-values dict))
  (define all-values
    (for/fold ([set (multiset-add-all old-all-values
                                      (set-delta-missing-elements delta))])
              ([v (in-immutable-set (set-delta-extra-elements delta))])
      (multiset-remove set v)))
  (constructor:multidict
   #:size size
   #:backing-hash backing-hash
   #:inverted-hash inverted-hash
   #:keys keys
   #:values all-values))

(define (update-inverted-hash-from-value-mappings-delta old-inverted-hash
                                                        k
                                                        delta)
  (define inverted-hash/removals-applied
    (for/fold ([h old-inverted-hash])
              ([v (in-immutable-set (set-delta-extra-elements delta))])
      (define new-ks (set-remove (hash-ref h v (set)) k))
      (if (set-empty? new-ks)
          (hash-remove h v)
          (hash-set h v new-ks))))
  (for/fold ([h inverted-hash/removals-applied])
            ([v (in-immutable-set (set-delta-missing-elements delta))])
    (hash-set h v (set-add (hash-ref h v (set)) k))))

(define/guard (multidict-add dict k v)
  (define old-vs (multidict-ref dict k))
  (guard (set-member? old-vs v) then dict)
  (define delta
    (set-delta #:extra-elements (set) #:missing-elements (set v)))
  (multidict-replace-values dict k (set-add old-vs v)
                            #:precomputed-value-set-delta delta))

(define (multidict-add-entry dict e)
  (multidict-add dict (entry-key e) (entry-value e)))

(define/guard (multidict-remove dict k v)
  (define old-vs (multidict-ref dict k))
  (guard (set-member? old-vs v) else dict)
  (define delta (set-delta #:extra-elements (set v) #:missing-elements (set)))
  (multidict-replace-values
   dict k (set-remove old-vs v) #:precomputed-value-set-delta delta))

(define (multidict-remove-entry dict e)
  (multidict-remove dict (entry-key e) (entry-value e)))

(module+ test

  (test-case "multidict-replace-values"
    (check-equal? (multidict-replace-values empty-multidict 'a (list 1 2 2 3))
                  (multidict 'a 1 'a 2 'a 3))
    (check-equal? (multidict-replace-values (multidict 'a 1) 'a (list 1 2 2 3))
                  (multidict 'a 1 'a 2 'a 3))
    (check-equal? (multidict-replace-values (multidict 'b 2) 'a (list 1 2 2 3))
                  (multidict 'a 1 'a 2 'a 3 'b 2))
    (check-equal?
     (multidict-replace-values (multidict 'a 1 'b 2) 'a (list 1 2 2 3))
     (multidict 'a 1 'a 2 'a 3 'b 2))
    (check-equal?
     (multidict-replace-values (multidict 'a 4 'b 2) 'a (list 1 2 2 3))
     (multidict 'a 1 'a 2 'a 3 'b 2)))

  (test-case "multidict-add"
    (check-equal? (multidict-add empty-multidict 'a 1) (multidict 'a 1))
    (check-equal? (multidict-add (multidict 'a 1) 'b 2) (multidict 'a 1 'b 2))
    (check-equal? (multidict-add (multidict 'a 1) 'a 2) (multidict 'a 1 'a 2))
    (check-equal? (multidict-add (multidict 'a 1) 'a 1) (multidict 'a 1)))

  (test-case "multidict-add-entry"
    (check-equal? (multidict-add-entry empty-multidict (entry 'a 1))
                  (multidict 'a 1))
    (check-equal? (multidict-add-entry (multidict 'a 1) (entry 'b 2))
                  (multidict 'a 1 'b 2))
    (check-equal? (multidict-add-entry (multidict 'a 1) (entry 'a 2))
                  (multidict 'a 1 'a 2))
    (check-equal? (multidict-add-entry (multidict 'a 1) (entry 'a 1))
                  (multidict 'a 1)))

  (test-case "multidict-remove"
    (check-equal? (multidict-remove (multidict 'a 1 'b 2) 'a 1)
                  (multidict 'b 2))
    (check-equal? (multidict-remove (multidict 'a 1 'a 2) 'a 1)
                  (multidict 'a 2))
    (check-equal? (multidict-remove (multidict 'a 1) 'a 1) empty-multidict)
    (check-equal? (multidict-remove (multidict 'a 1 'b 2) 'c 3)
                  (multidict 'a 1 'b 2)))

  (test-case "multidict-remove-entry"
    (check-equal? (multidict-remove-entry (multidict 'a 1 'b 2) (entry 'a 1))
                  (multidict 'b 2))
    (check-equal? (multidict-remove-entry (multidict 'a 1 'a 2) (entry 'a 1))
                  (multidict 'a 2))
    (check-equal? (multidict-remove-entry (multidict 'a 1) (entry 'a 1))
                  empty-multidict)
    (check-equal? (multidict-remove-entry (multidict 'a 1 'b 2) (entry 'c 3))
                  (multidict 'a 1 'b 2))))

(define into-multidict
  (make-fold-reducer multidict-add-entry
                     empty-multidict
                     #:name 'into-multidict))

(define-syntaxes (for/multidict for*/multidict)
  (make-reducer-based-for-comprehensions #'into-multidict))

(define (multidict . entries)
  (for/multidict ([entry-pair (in-slice 2 entries)])
    (entry (first entry-pair) (second entry-pair))))

(define (multidict-unique-keys dict)
  (multiset-unique-elements (multidict-keys dict)))

(define (multidict-unique-values dict)
  (multiset-unique-elements (multidict-values dict)))

(define (multidict-entries dict)
  (for/set ([e (in-multidict-entries dict)]) e))

(define (multidict->hash dict) (multidict-backing-hash dict))

(define (multidict-inverse dict)
  (constructor:multidict
   #:size (multidict-size dict)
   #:backing-hash (multidict-inverted-hash dict)
   #:inverted-hash (multidict-backing-hash dict)
   #:keys (multidict-values dict)
   #:values (multidict-keys dict)))

(define (multidict-ref dict k) (hash-ref (multidict-backing-hash dict) k (set)))

(define (multidict-contains-key? dict k)
  (hash-has-key? (multidict-backing-hash dict) k))

(define (multidict-contains-value? dict v)
  (for/or ([vs (in-immutable-hash-values (multidict-backing-hash dict))])
    (set-member? vs v)))

(define (multidict-contains-entry? dict e)
  (set-member? (multidict-ref dict (entry-key e)) (entry-value e)))

(define (empty-multidict? v) (equal? v empty-multidict))

(define (nonempty-multidict? v)
  (and (multidict? v) (not (empty-multidict? v))))

(module+ test
  (define dict
    (multidict 'a 1
               'a 2
               'b 3
               'c 4
               'a 2
               'c 1))
  (test-case "multidict-ref"
    (check-equal? (multidict-ref dict 'a) (set 1 2))
    (check-equal? (multidict-ref dict 'b) (set 3))
    (check-equal? (multidict-ref dict 'c) (set 1 4))
    (check-equal? (multidict-ref dict 'd) (set)))
  (test-case "multidict-keys"
    (check-equal? (multidict-keys dict) (multiset 'a 'a 'b 'c 'c)))
  (test-case "multidict-values"
    (check-equal? (multidict-values dict) (multiset 1 2 3 4 1)))
  (test-case "multidict-unique-keys"
    (check-equal? (multidict-unique-keys dict) (set 'a 'b 'c)))
  (test-case "multidict-unique-values"
    (check-equal? (multidict-unique-values dict) (set 1 2 3 4)))
  (test-case "multidict-entries"
    (check-equal? (multidict-entries dict)
                  (set (entry 'a 1)
                       (entry 'a 2)
                       (entry 'b 3)
                       (entry 'c 4)
                       (entry 'c 1))))
  (test-case "multidict-size"
    (check-equal? (multidict-size dict) 5))
  (test-case "multidict->hash"
    (check-equal? (multidict->hash dict)
                  (hash 'a (set 1 2)
                        'b (set 3)
                        'c (set 1 4))))
  (test-case "multidict-inverse"
    (check-equal? (multidict-inverse dict)
                  (multidict 1 'a
                             2 'a
                             3 'b
                             4 'c
                             2 'a
                             1 'c))
    (check-equal? (multidict-inverse (multidict-inverse dict)) dict))
  (test-case "multidict-contains-key?"
    (check-true (multidict-contains-key? dict 'a))
    (check-true (multidict-contains-key? dict 'b))
    (check-true (multidict-contains-key? dict 'c))
    (check-false (multidict-contains-key? dict 'd)))
  (test-case "multidict-contains-value?"
    (check-true (multidict-contains-value? dict 1))
    (check-true (multidict-contains-value? dict 2))
    (check-true (multidict-contains-value? dict 3))
    (check-true (multidict-contains-value? dict 4))
    (check-false (multidict-contains-value? dict 5)))
  (test-case "multidict-contains-entry?"
    (check-true (multidict-contains-entry? dict (entry 'a 1)))
    (check-true (multidict-contains-entry? dict (entry 'a 2)))
    (check-true (multidict-contains-entry? dict (entry 'b 3)))
    (check-false (multidict-contains-entry? dict (entry 'a 3)))
    (check-false (multidict-contains-entry? dict (entry 'd 1)))
    (check-false (multidict-contains-entry? dict (entry 'a 5)))))
