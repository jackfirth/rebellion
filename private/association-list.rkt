#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [association-list (->* () #:rest key-value-list/c association-list?)]
  [association-list? predicate/c]
  [association-list-ref (-> association-list? any/c immutable-vector?)]
  [association-list-size (-> association-list? natural?)]
  [association-list-keys (-> association-list? multiset?)]
  [association-list-unique-keys (-> association-list? immutable-set?)]
  [association-list-values (-> association-list? immutable-vector?)]
  [association-list-entries
   (-> association-list? (vectorof entry? #:immutable #t #:flat? #t))]
  [association-list->hash
   (-> association-list?
       (hash/c any/c nonempty-immutable-vector?
               #:immutable #t
               #:flat? #t))]
  [association-list-contains-key? (-> association-list? any/c boolean?)]
  [association-list-contains-value? (-> association-list? any/c boolean?)]
  [association-list-contains-entry? (-> association-list? entry? boolean?)]
  [empty-association-list association-list?]
  [empty-association-list? predicate/c]
  [nonempty-association-list? predicate/c]))

(require racket/list
         racket/math
         racket/sequence
         racket/set
         racket/struct
         rebellion/collection/entry
         rebellion/collection/immutable-vector
         rebellion/collection/keyset
         rebellion/collection/multiset
         rebellion/private/spliced-printing-entry
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

(define (immutable-vector-contains? vec v)
  (not (not (immutable-vector-member v vec))))

(define (immutable-vector-index-of vec v) (immutable-vector-member v vec))

;@------------------------------------------------------------------------------

(define (make-association-list-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define accessor (record-descriptor-accessor descriptor))
  (define backing-hash-field
    (keyset-index-of (record-type-fields type) '#:backing-hash))
  (define equal+hash (make-record-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) type-name)
     (λ (this)
       (define backing-hash (accessor this backing-hash-field))
       (for*/list ([(k vs) (in-immutable-hash backing-hash)]
                   [v (in-vector vs)])
         (spliced-printing-entry k v)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-record-type association-list (backing-hash size)
  #:constructor-name constructor:association-list
  #:property-maker make-association-list-properties)

(define (association-list . entries)
  (define-values (backing-list-hash size)
    (for/fold ([h (hash)] [size 0])
              ([e (in-slice 2 entries)])
      (define k (first e))
      (define v (second e))
      (values (hash-update h k (λ (lst) (cons v lst)) '())
              (add1 size))))
  (define backing-hash
    (for/hash ([(k lst) (in-hash backing-list-hash)])
      (values k (list->immutable-vector (reverse lst)))))
  (constructor:association-list #:backing-hash backing-hash #:size size))

(define (association-list-ref assoc k)
  (hash-ref (association-list-backing-hash assoc) k empty-immutable-vector))

(define (association-list-keys assoc)
  (for*/multiset ([(k vec) (in-hash (association-list-backing-hash assoc))]
                  [_ (in-range (immutable-vector-length vec))])
    k))

(define (association-list-unique-keys assoc)
  (multiset-unique-elements (association-list-keys assoc)))

(define (association-list-values assoc)
  (vector->immutable-vector
   (for*/vector #:length (association-list-size assoc)
     ([vec (in-hash-values (association-list-backing-hash assoc))]
      [v (in-vector vec)])
     v)))

(define (association-list-entries assoc)
  (vector->immutable-vector
   (for*/vector #:length (association-list-size assoc)
     ([(k vec) (in-hash (association-list-backing-hash assoc))]
      [v (in-vector vec)])
     (entry k v))))

(define (association-list->hash assoc)
  (association-list-backing-hash assoc))

(define empty-association-list (association-list))

(define (empty-association-list? v) (equal? v empty-association-list))

(define (nonempty-association-list? v)
  (and (association-list? v) (not (equal? v empty-association-list))))

(define (association-list-contains-key? assoc k)
  (hash-has-key? (association-list-backing-hash assoc) k))

(define (association-list-contains-value? assoc v)
  (define backing-hash (association-list-backing-hash assoc))
  (for/or ([vec (in-immutable-hash-values backing-hash)])
    (immutable-vector-contains? vec v)))

(define (association-list-contains-entry? assoc e)
  (immutable-vector-contains? (association-list-ref assoc (entry-key e))
                              (entry-value e)))

(module+ test
  (define assoc (association-list 'a 1 'b 2 'a 3 'c 4))
  (define alt-assoc (association-list 'a 3 'b 2 'a 1 'c 4))

  (test-case "association-list-ref"
    (check-equal? (association-list-ref assoc 'a) (immutable-vector 1 3))
    (check-equal? (association-list-ref assoc 'b) (immutable-vector 2))
    (check-equal? (association-list-ref assoc 'd) empty-immutable-vector)
    (check-not-equal? (association-list-ref assoc 'a)
                      (association-list-ref alt-assoc 'a)))
  
  (test-case "association-list-size"
    (check-equal? (association-list-size assoc) 4))
  
  (test-case "association-list-keys"
    (check-equal? (association-list-keys assoc) (multiset 'a 'a 'b 'c)))
  
  (test-case "association-list-unique-keys"
    (check-equal? (association-list-unique-keys assoc) (set 'a 'b 'c)))
  
  (test-case "association-list-values"
    (define vs (association-list-values assoc))
    (check-equal? (immutable-vector-length vs) 4)
    (check-true (immutable-vector-contains? vs 1))
    (check-true (immutable-vector-contains? vs 2))
    (check-true (immutable-vector-contains? vs 3))
    (check-true (immutable-vector-contains? vs 4))
    (check-true (< (immutable-vector-index-of vs 1)
                   (immutable-vector-index-of vs 3)))
    (check-not-equal? vs (association-list-values alt-assoc)))
  
  (test-case "association-list-entries"
    (define entries (association-list-entries assoc))
    (check-equal? (immutable-vector-length entries) 4)
    (check-true (immutable-vector-contains? entries (entry 'a 1)))
    (check-true (immutable-vector-contains? entries (entry 'a 3)))
    (check-true (immutable-vector-contains? entries (entry 'b 2)))
    (check-true (immutable-vector-contains? entries (entry 'c 4)))
    (check-true (< (immutable-vector-index-of entries (entry 'a 1))
                   (immutable-vector-index-of entries (entry 'a 3))))
    (check-not-equal? entries (association-list-entries alt-assoc)))
  
  (test-case "association-list->hash"
    (check-equal? (association-list->hash assoc)
                  (hash 'a (immutable-vector 1 3)
                        'b (immutable-vector 2)
                        'c (immutable-vector 4))))

  (test-case "association-list-contains-key?"
    (check-true (association-list-contains-key? assoc 'a))
    (check-false (association-list-contains-key? assoc 'foo))
    (check-false (association-list-contains-key? empty-association-list 'a)))

  (test-case "association-list-contains-value?"
    (check-true (association-list-contains-value? assoc 3))
    (check-false (association-list-contains-value? assoc 1000))
    (check-false (association-list-contains-value? empty-association-list 3)))

  (test-case "association-list-contains-entry?"
    (check-true (association-list-contains-entry? assoc (entry 'a 3)))
    (check-false (association-list-contains-entry? assoc (entry 'a 1000)))
    (check-false (association-list-contains-entry? assoc (entry 'foo 3)))
    (check-false (association-list-contains-entry? empty-association-list
                                                   (entry 'a 2)))))
