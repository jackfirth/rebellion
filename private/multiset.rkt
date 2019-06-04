#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multiset (-> any/c ... multiset?)]
  [multiset? (-> any/c boolean?)]
  [multiset-contains? (-> multiset? any/c boolean?)]
  [multiset-frequency (-> multiset? any/c natural?)]
  [multiset-frequencies
   (-> multiset? (immutable-hash/c any/c exact-positive-integer?))]
  [multiset-size (-> multiset? natural?)]
  [multiset-unique-elements (-> multiset? immutable-set?)]
  [multiset->list (-> multiset? list?)]
  [list->multiset (-> list? multiset?)]
  [empty-multiset multiset?]))

(require racket/math
         racket/set
         racket/struct
         rebellion/equal+hash/tuple
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (immutable-hash/c key-contract value-contract)
  (define both-flat?
    (and (flat-contract? key-contract) (flat-contract? value-contract)))
  (hash/c key-contract value-contract #:immutable #t #:flat? both-flat?))

(define (immutable-set? v) (and (set? v) (not (set-mutable? v))))

;@------------------------------------------------------------------------------

(define (make-multiset-props descriptor)
  (define name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define equal+hash (make-tuple-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this) (frequency-hash->list (accessor this 1)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-tuple-type multiset (size frequencies)
  #:property-maker make-multiset-props
  #:constructor constructor:multiset)

(define empty-hash (hash))

(define (multiset . vs)
  (for/fold ([h empty-hash]
             [size 0]
             #:result (constructor:multiset size h))
            ([v (in-list vs)])
    (values (hash-set h v (add1 (hash-ref h v 0)))
            (add1 size))))

(define (multiset-frequency set elem)
  (hash-ref (multiset-frequencies set) elem 0))

(define (multiset->list set)
  (frequency-hash->list (multiset-frequencies set)))

(define (list->multiset lst) (apply multiset lst))

(define (multiset-unique-elements set)
  (list->set (hash-keys (multiset-frequencies set))))

(define (multiset-contains? set v)
  (hash-has-key? (multiset-frequencies set) v))

(define (frequency-hash->list frequencies)
  (for*/list ([(elem freq) (in-hash frequencies)]
              [_ (in-range freq)])
    elem))

(define empty-multiset (multiset))

(module+ test
  (define letters (multiset 'a 'b 'b 'b 'c 'c 'd))
  (check-equal? (multiset-frequencies letters) (hash 'a 1 'b 3 'c 2 'd 1))
  (check-equal? (multiset-frequency letters 'b) 3)
  (check-equal? (multiset-frequency letters 'foo) 0)
  (check-equal? (multiset-size letters) 7)
  (check-equal? (multiset-unique-elements letters) (set 'a 'b 'c 'd))
  (check-true (multiset-contains? letters 'a))
  (check-true (multiset-contains? letters 'b))
  (check-false (multiset-contains? letters 'foo))
  (check-equal? (list->multiset (multiset->list letters)) letters)
  (check-equal? (length (multiset->list letters)) 7))
