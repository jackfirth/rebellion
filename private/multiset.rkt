#lang racket/base

(require racket/contract/base)

(provide
 for/multiset
 for*/multiset
 (contract-out
  [multiset (-> any/c ... multiset?)]
  [multiset? (-> any/c boolean?)]
  [multiset-add (->* (multiset? any/c) (#:copies natural?) multiset?)]
  [multiset-add-all (-> multiset? multiset-coercible-sequence/c multiset?)]
  [multiset-remove
   (->* (multiset? any/c) (#:copies (or/c natural? +inf.0)) multiset?)]
  [multiset-set-frequency (-> multiset? any/c natural? multiset?)]
  [multiset-contains? (-> multiset? any/c boolean?)]
  [multiset-frequency (-> multiset? any/c natural?)]
  [multiset-frequencies
   (-> multiset? (immutable-hash/c any/c exact-positive-integer?))]
  [multiset-size (-> multiset? natural?)]
  [multiset-unique-elements (-> multiset? set?)]
  [multiset->list (-> multiset? list?)]
  [sequence->multiset (-> multiset-coercible-sequence/c multiset?)]
  [empty-multiset multiset?]
  [in-multiset (-> multiset? sequence?)]
  [into-multiset reducer?]))

(require (for-syntax racket/base)
         racket/hash
         racket/math
         racket/set
         racket/sequence
         racket/stream
         racket/struct
         rebellion/streaming/reducer
         rebellion/type/record)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (immutable-hash/c key-contract value-contract)
  (define both-flat?
    (and (flat-contract? key-contract) (flat-contract? value-contract)))
  (hash/c key-contract value-contract #:immutable #t #:flat? both-flat?))

(define (set? v) (and (set? v) (not (set-mutable? v))))

;@------------------------------------------------------------------------------

(define (in-multiset set)
  (for*/stream ([(v count) (in-immutable-hash (multiset-frequencies set))]
                [_ (in-range count)])
    v))

(define (make-multiset-props descriptor)
  (define name (record-type-name (record-descriptor-type descriptor)))
  (define equal+hash (make-record-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this) (in-multiset this))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)
        (cons prop:sequence in-multiset)))

(define-record-type multiset (size frequencies unique-elements)
  #:property-maker make-multiset-props
  #:constructor-name constructor:multiset)

(define empty-hash (hash))
(define empty-set (set))

(define (multiset . vs)
  (for/multiset ([v (in-list vs)])
    v))

(define empty-multiset
  (constructor:multiset #:size 0
                        #:frequencies empty-hash
                        #:unique-elements empty-set))

(define (multiset-set-frequency set element frequency)
  (define old-frequency (multiset-frequency set element))
  (define old-unique-elements (multiset-unique-elements set))
  (define frequencies
    (if (zero? frequency)
        (hash-remove (multiset-frequencies set) element)
        (hash-set (multiset-frequencies set) element frequency)))
  (define unique-elements
    (cond
      [(and (zero? old-frequency) (not (zero? frequency)))
       (set-add old-unique-elements element)]
      [(and (not (zero? old-frequency)) (zero? frequency))
       (set-remove old-unique-elements element)]
      [else old-unique-elements]))
  (constructor:multiset
   #:size (+ (multiset-size set) frequency (- old-frequency))
   #:frequencies frequencies
   #:unique-elements unique-elements))

(define (multiset-add set element #:copies [copies 1])
  (define frequency (+ (multiset-frequency set element) copies))
  (multiset-set-frequency set element frequency))

(define (multiset-remove set element #:copies [copies 1])
  (cond
    [(not (multiset-contains? set element)) set]
    [(equal? copies +inf.0) (multiset-set-frequency set element 0)]
    [else
     (define frequency (max (- (multiset-frequency set element) copies) 0))
     (multiset-set-frequency set element frequency)]))

(module+ test
  (test-case "multiset-add"

    (test-case "not-already-present"
      (define set (multiset 'a 'a 'b))
      (check-equal? (multiset-add set 'c) (multiset 'a 'a 'b 'c))
      (check-equal? (multiset-add set 'c #:copies 3)
                    (multiset 'a 'a 'b 'c 'c 'c)))

    (test-case "already-present"
      (define set (multiset 'a 'a 'b))
      (check-equal? (multiset-add set 'a) (multiset 'a 'a 'a 'b))
      (check-equal? (multiset-add set 'b) (multiset 'a 'a 'b 'b))
      (check-equal? (multiset-add set 'b #:copies 3)
                    (multiset 'a 'a 'b 'b 'b 'b)))

    (test-case "zero-copies"
      (define set (multiset 'a 'b 'b 'c))
      (check-equal? (multiset-add set 'a #:copies 0) set)
      (check-equal? (multiset-add set 'd #:copies 0) set)))

  (test-case "add-all"
    (check-equal? (multiset-add-all (multiset 1 1 2 3) (in-range 0 5))
                  (multiset 0 1 1 1 2 2 3 3 4))
    (check-equal? (multiset-add-all (multiset 1 2 3) (multiset 1 2 3))
                  (multiset 1 1 2 2 3 3)))

  (test-case "multiset-set-frequency"
    
    (test-case "clearing-already-empty"
      (define set (multiset-set-frequency empty-multiset 'a 0))
      (check-equal? (multiset-size set) 0))

    (test-case "setting-empty-to-one"
      (define set (multiset-set-frequency empty-multiset 'a 1))
      (check-equal? (multiset-size set) 1))

    (test-case "setting-empty-to-many"
      (define set (multiset-set-frequency empty-multiset 'a 5))
      (check-equal? (multiset-size set) 5))
      
    (test-case "one-unique-element"
      (define set (multiset 'a 'a 'a))
      (check-equal? (multiset-size (multiset-set-frequency set 'a 0)) 0)
      (check-equal? (multiset-size (multiset-set-frequency set 'a 1)) 1)
      (check-equal? (multiset-size (multiset-set-frequency set 'a 3)) 3)
      (check-equal? (multiset-size (multiset-set-frequency set 'a 5)) 5))

    (test-case "setting-to-zero"
      (define set (multiset 'a 'a 'b 'b 'b 'c))
      (define (set-to-zero element) (multiset-set-frequency set element 0))
      (check-equal? (multiset-size set) 6)
      (check-equal? (multiset-size (set-to-zero 'a)) 4)
      (check-equal? (multiset-size (set-to-zero 'b)) 3)
      (check-equal? (multiset-size (set-to-zero 'c)) 5)
      (check-equal? (multiset-size (set-to-zero 'd)) 6))
    
    (test-case "equality"
      (define set (multiset 'a 'b 'c))
      (check-equal? (multiset-size (multiset-set-frequency set 'a 3)) 5)
      (check-equal? (multiset-set-frequency set 'a 3) (multiset 'a 'a 'a 'b 'c))
      (check-equal? (multiset-set-frequency set 'a 1) set)
      (check-equal? (multiset-set-frequency set 'a 0) (multiset 'b 'c))
      (check-equal? (multiset-set-frequency set 'd 2) (multiset 'a 'b 'c 'd 'd))
      (check-equal? (multiset-set-frequency set 'd 0) set)))
  
  (test-case "multiset-remove"

    (test-case "single-copy"
      (define set (multiset 'a 'b 'b 'c))
      (check-equal? (multiset-remove set 'a) (multiset 'b 'b 'c))
      (check-equal? (multiset-remove set 'b) (multiset 'a 'b 'c))
      (check-equal? (multiset-remove set 'd) set))
    
    (test-case "multiple-copies"
      (define set (multiset 'a 'a 'b 'b 'b 'c))
      (check-equal? (multiset-remove set 'a #:copies 2) (multiset 'b 'b 'b 'c))
      (check-equal? (multiset-remove set 'b #:copies 2) (multiset 'a 'a 'b 'c))
      (check-equal? (multiset-remove set 'c #:copies 2)
                    (multiset 'a 'a 'b 'b 'b))
      (check-equal? (multiset-remove set 'b #:copies 3) (multiset 'a 'a 'c))
      (check-equal? (multiset-remove set 'd #:copies 5) set))
    
    (test-case "zero-copies"
      (define set (multiset 'a 'b 'b 'c))
      (check-equal? (multiset-remove set 'a #:copies 0) set)
      (check-equal? (multiset-remove set 'b #:copies 0) set)
      (check-equal? (multiset-remove set 'd #:copies 0) set))
    
    (test-case "all-copies"
      (define set (multiset 'a 'b 'b 'c))
      (check-equal? (multiset-remove set 'a #:copies +inf.0)
                    (multiset 'b 'b 'c))
      (check-equal? (multiset-remove set 'b #:copies +inf.0) (multiset 'a 'c))
      (check-equal? (multiset-remove set 'd #:copies +inf.0) set))))

(define (multiset-union a b)
  (constructor:multiset
   #:size (+ (multiset-size a) (multiset-size b))
   #:frequencies (hash-union (multiset-frequencies a)
                             (multiset-frequencies b)
                             #:combine +)
   #:unique-elements (set-union (multiset-unique-elements a)
                                (multiset-unique-elements b))))

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

(define multiset-coercible-sequence/c (or/c multiset? (sequence/c any/c)))

(define (sequence->multiset seq)
  (if (multiset? seq) seq (reduce-all into-multiset seq)))

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
    (check-equal? (sequence->multiset "hello")
                  (multiset #\h #\e #\l #\l #\o)))
  (test-case "iteration"
    (check-equal? (for/multiset ([char (in-string "hello")]) char)
                  (multiset #\h #\e #\l #\l #\o))
    (check-equal? (for/set ([letter (in-multiset letters)]) letter)
                  (set 'a 'b 'c 'd))))
