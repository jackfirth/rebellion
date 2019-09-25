#lang racket/base

(require racket/contract/base)

(provide
 for/multiset
 for*/multiset
 (contract-out
  [multiset (-> any/c ... multiset?)]
  [multiset? (-> any/c boolean?)]
  [multiset-add (-> multiset? any/c multiset?)]
  [multiset-add-all (-> multiset? (or/c multiset? (sequence/c any/c))
                        multiset?)]
  [multiset-remove-once (-> multiset? any/c multiset?)]
  [multiset-contains? (-> multiset? any/c boolean?)]
  [multiset-frequency (-> multiset? any/c natural?)]
  [multiset-frequencies
   (-> multiset? (immutable-hash/c any/c exact-positive-integer?))]
  [multiset-size (-> multiset? natural?)]
  [multiset-unique-elements (-> multiset? immutable-set?)]
  [multiset->list (-> multiset? list?)]
  [list->multiset (-> list? multiset?)]
  [sequence->multiset (-> (sequence/c any/c) multiset?)]
  [empty-multiset multiset?]
  [in-multiset (-> multiset? sequence?)]
  [into-multiset reducer?]))

(require (for-syntax racket/base)
         racket/math
         racket/set
         racket/sequence
         racket/stream
         racket/struct
         rebellion/streaming/reducer
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

(define (in-multiset set)
  (for*/stream ([(v count) (in-immutable-hash (multiset-frequencies set))]
                [_ (in-range count)])
    v))

(define (make-multiset-props descriptor)
  (define name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define equal+hash (make-tuple-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this) (frequency-hash->list (accessor this 1)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)
        (cons prop:sequence in-multiset)))

(define-tuple-type multiset (size frequencies)
  #:property-maker make-multiset-props
  #:constructor-name constructor:multiset)

(define empty-hash (hash))

(define (multiset . vs)
  (for/multiset ([v (in-list vs)])
    v))

(define empty-multiset (constructor:multiset 0 empty-hash))

(define (multiset-add set v)
  (constructor:multiset (add1 (multiset-size set))
                        (hash-update (multiset-frequencies set) v add1 0)))

(define (multiset-remove-once set v)
  (define freq (multiset-frequency set v))
  (cond
    [(zero? freq) set]
    [(= 1 freq)
     (constructor:multiset (sub1 (multiset-size set))
                           (hash-remove (multiset-frequencies set) v))]
    [else
     (constructor:multiset (sub1 (multiset-size set))
                           (hash-update (multiset-frequencies set) v sub1))]))

(define (multiset-union a b)
  (define a-freqs (multiset-frequencies a))
  (define b-freqs (multiset-frequencies b))
  (define vals (set-union (list->set (hash-keys a-freqs))
                          (list->set (hash-keys b-freqs))))
  (define union-freqs
    (for/hash ([v (in-set vals)])
      (values v (+ (hash-ref a-freqs v 0) (hash-ref b-freqs v 0)))))
  (constructor:multiset (+ (multiset-size a) (multiset-size b)) union-freqs))

(define (multiset-add-all set seq)
  (multiset-union set
                  (if (multiset? seq)
                      seq
                      (sequence->multiset seq))))

(define into-multiset
  (make-fold-reducer multiset-add empty-multiset #:name 'into-multiset))

(define-syntaxes (for/multiset for*/multiset)
  (make-reducer-based-for-comprehensions #'into-multiset))

(define (multiset-frequency set elem)
  (hash-ref (multiset-frequencies set) elem 0))

(define (multiset->list set)
  (frequency-hash->list (multiset-frequencies set)))

(define (list->multiset lst) (apply multiset lst))

(define (sequence->multiset seq)
  (reduce-all into-multiset seq))

(define (multiset-unique-elements set)
  (list->set (hash-keys (multiset-frequencies set))))

(define (multiset-contains? set v)
  (hash-has-key? (multiset-frequencies set) v))

(define (frequency-hash->list frequencies)
  (for*/list ([(elem freq) (in-hash frequencies)]
              [_ (in-range freq)])
    elem))

(module+ test
  (define letters (multiset 'a 'b 'b 'b 'c 'c 'd))
  (test-case "queries"
    (check-equal? (multiset-frequencies letters) (hash 'a 1 'b 3 'c 2 'd 1))
    (check-equal? (multiset-frequency letters 'b) 3)
    (check-equal? (multiset-frequency letters 'foo) 0)
    (check-equal? (multiset-size letters) 7)
    (check-equal? (multiset-unique-elements letters) (set 'a 'b 'c 'd))
    (check-true (multiset-contains? letters 'a))
    (check-true (multiset-contains? letters 'b))
    (check-false (multiset-contains? letters 'foo)))
  (test-case "conversion"
    (check-equal? (list->multiset (multiset->list letters)) letters)
    (check-equal? (length (multiset->list letters)) 7)
    (check-equal? (sequence->multiset "hello")
                  (multiset #\h #\e #\l #\l #\o)))
  (test-case "iteration"
    (check-equal? (for/multiset ([char (in-string "hello")]) char)
                  (multiset #\h #\e #\l #\l #\o))
    (check-equal? (for/set ([letter (in-multiset letters)]) letter)
                  (set 'a 'b 'c 'd)))
  (test-case "add-all"
    (check-equal? (multiset-add-all (multiset 1 1 2 3) (in-range 0 5))
                  (multiset 0 1 1 1 2 2 3 3 4))
    (check-equal? (multiset-add-all (multiset 1 2 3) (multiset 1 2 3))
                  (multiset 1 1 2 2 3 3)))
  (test-case "removal"
    (let ([set (multiset-remove-once letters 'b)])
      (check-equal? (multiset-frequency set 'b) 2)
      (check-equal? (multiset-size set) 6))
    (let ([set (multiset-remove-once letters 'a)])
      (check-equal? (multiset-frequency set 'a) 0)
      (check-equal? (multiset-size set) 6))
    (let ([set (multiset-remove-once letters 'foo)])
      (check-equal? (multiset-frequency set 'foo) 0)
      (check-equal? (multiset-size set) 7))
    (let ([set (multiset-remove-once (multiset) 'a)])
      (check-equal? (multiset-frequency set 'a) 0)
      (check-equal? (multiset-size set) 0))))
