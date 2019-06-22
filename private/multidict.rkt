#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multidict (->* () #:rest key-value-list/c multidict?)]
  [multidict? predicate/c]
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
  [empty-multidict empty-multidict?]
  [empty-multidict? predicate/c]
  [nonempty-multidict? predicate/c]))

(require racket/list
         racket/math
         racket/sequence
         racket/set
         racket/struct
         rebellion/collection/entry
         rebellion/collection/multiset
         rebellion/collection/keyset
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
       (define backing-hash (accessor this backing-hash-field))
       (for*/list ([(k vs) (in-immutable-hash backing-hash)]
                   [v (in-immutable-set vs)]
                   [k-or-v (in-list (list k v))])
         k-or-v))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-record-type multidict (backing-hash size)
  #:constructor-name constructor:multidict
  #:property-maker make-multidict-properties)

(define (multidict . entries)
  (define backing-hash
    (for/fold ([h (hash)])
              ([e (in-slice 2 entries)])
      (define k (first e))
      (define v (second e))
      (hash-update h k (λ (st) (set-add st v)) (set))))
  (define size
    (for/sum ([vs (in-immutable-hash-values backing-hash)])
      (set-count vs)))
  (constructor:multidict #:backing-hash backing-hash #:size size))

(define (multidict-keys dict)
  (list->multiset
   (for*/list ([(k vs) (in-immutable-hash (multidict-backing-hash dict))]
               [_ (in-range (set-count vs))])
     k)))

(define (multidict-unique-keys dict)
  (list->set (hash-keys (multidict-backing-hash dict))))

(define (multidict-values dict)
  (list->multiset
   (for*/list ([vs (in-immutable-hash-values (multidict-backing-hash dict))]
               [v (in-immutable-set vs)])
     v)))

(define (multidict-entries dict)
  (for*/set ([(k vs) (in-immutable-hash (multidict-backing-hash dict))]
             [v (in-immutable-set vs)])
    (entry k v)))

(define (multidict->hash dict) (multidict-backing-hash dict))

(define (multidict-ref dict k) (hash-ref (multidict-backing-hash dict) k (set)))

(define (multidict-contains-key? dict k)
  (hash-has-key? (multidict-backing-hash dict) k))

(define (multidict-contains-value? dict v)
  (for/or ([vs (in-immutable-hash-values (multidict-backing-hash dict))])
    (set-member? vs v)))

(define (multidict-contains-entry? dict e)
  (set-member? (multidict-ref dict (entry-key e)) (entry-value e)))

(define empty-multidict (multidict))

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
