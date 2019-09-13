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
  [multidict-size (-> multidict? natural?)]
  [multidict-ref (-> multidict? any/c immutable-set?)]
  [multidict-keys (-> multidict? multiset?)]
  [multidict-values (-> multidict? multiset?)]
  [multidict-unique-keys (-> multidict? immutable-set?)]
  [multidict-entries (-> multidict? (set/c entry? #:cmp 'equal))]
  [multidict-contains-key? (-> multidict? any/c boolean?)]
  [multidict-contains-value? (-> multidict? any/c boolean?)]
  [multidict-contains-entry? (-> multidict? entry? boolean?)]
  [multidict->hash
   (-> multidict?
       (hash/c any/c nonempty-immutable-set? #:immutable #t #:flat? #t))]
  [multidict-inverse (-> multidict? multidict?)]
  [empty-multidict empty-multidict?]
  [empty-multidict? predicate/c]
  [nonempty-multidict? predicate/c]
  [in-multidict-entries (-> multidict? (sequence/c entry?))]
  [into-multidict reducer?]))

(require (for-syntax racket/base)
         racket/list
         racket/math
         racket/sequence
         racket/set
         racket/stream
         racket/struct
         rebellion/collection/entry
         rebellion/collection/multiset
         rebellion/collection/keyset
         rebellion/private/spliced-printing-entry
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

(define (immutable-set? v) (and (set? v) (not (set-mutable? v))))

(define (nonempty-immutable-set? v)
  (and (immutable-set? v) (not (zero? (set-count v)))))

(define (make-multidict-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define backing-hash-field
    (keyset-index-of (record-type-fields type) '#:backing-hash))
  (define accessor (record-descriptor-accessor descriptor))
  (define equal+hash (make-record-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) type-name)
     (λ (this)
       (for*/list ([e (sequence this)])
         (spliced-printing-entry (entry-key e) (entry-value e))))))
  (define (sequence this)
    (define backing-hash (accessor this backing-hash-field))
    (for*/stream ([(k vs) (in-immutable-hash backing-hash)]
                  [v (in-immutable-set vs)])
      (entry k v)))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:sequence sequence)
        (cons prop:custom-write custom-write)))

(define-record-type multidict (backing-hash size)
  #:constructor-name constructor:multidict
  #:property-maker make-multidict-properties)

(define (in-multidict-entries dict) dict)

(define empty-multidict (constructor:multidict #:backing-hash (hash) #:size 0))

(define (multidict-add dict k v)
  (define backing-hash (multidict-backing-hash dict))
  (define size (multidict-size dict))
  (define k-vs (hash-ref backing-hash k (set)))
  (if (set-member? k-vs v)
      dict
      (constructor:multidict
       #:backing-hash (hash-set backing-hash k (set-add k-vs v))
       #:size (add1 size))))

(define (multidict-add-entry dict e)
  (multidict-add dict (entry-key e) (entry-value e)))

(define into-multidict
  (make-fold-reducer multidict-add-entry
                     empty-multidict
                     #:name 'into-multidict))

(define-syntaxes (for/multidict for*/multidict)
  (make-reducer-based-for-comprehensions #'into-multidict))

(define (multidict . entries)
  (for/multidict ([entry-pair (in-slice 2 entries)])
    (entry (first entry-pair) (second entry-pair))))

(define (multidict-keys dict)
  (for/multiset ([e (in-multidict-entries dict)]) (entry-key e)))

(define (multidict-unique-keys dict)
  (for/set ([k (in-hash-keys (multidict-backing-hash dict))]) k))

(define (multidict-values dict)
  (for/multiset ([e (in-multidict-entries dict)]) (entry-value e)))

(define (multidict-entries dict)
  (for/set ([e (in-multidict-entries dict)]) e))

(define (multidict->hash dict) (multidict-backing-hash dict))

(define (multidict-inverse dict)
  (for/multidict ([e (in-multidict-entries dict)])
    (entry (entry-value e) (entry-key e))))

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
