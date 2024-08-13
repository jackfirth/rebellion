#lang racket/base


(require racket/contract/base)


(provide
 for/multiset
 for*/multiset
 (contract-out
  [multiset (-> any/c ... multiset?)]
  [multiset-of-frequencies (-> (entry/c any/c natural?) ... multiset?)]
  [multiset? (-> any/c boolean?)]
  [multiset-add (->* (multiset? any/c) (#:copies natural?) multiset?)]
  [multiset-add-all (-> multiset? multiset-coercible-sequence/c multiset?)]
  [multiset-remove (->* (multiset? any/c) (#:copies (or/c natural? +inf.0)) multiset?)]
  [multiset-remove-all (-> multiset? multiset-coercible-sequence/c multiset?)]
  [multiset-set-frequency (-> multiset? any/c natural? multiset?)]
  [multiset-contains? (-> multiset? any/c boolean?)]
  [multiset-contains-all? (-> multiset? multiset-coercible-sequence/c boolean?)]
  [multiset-contains-any? (-> multiset? multiset-coercible-sequence/c boolean?)]
  [multiset-contains-none? (-> multiset? multiset-coercible-sequence/c boolean?)]
  [multiset-frequency (-> multiset? any/c natural?)]
  [multiset-frequencies (-> multiset? (immutable-hash/c any/c exact-positive-integer?))]
  [multiset-size (-> multiset? natural?)]
  [multiset-unique-elements (-> multiset? set?)]
  [multiset->list (-> multiset? list?)]
  [sequence->multiset (-> multiset-coercible-sequence/c multiset?)]
  [empty-multiset multiset?]
  [in-multiset (-> multiset? sequence?)]
  [into-multiset (reducer/c any/c multiset?)]))


(require (for-syntax racket/base)
         racket/hash
         racket/math
         racket/set
         racket/sequence
         racket/stream
         racket/struct
         rebellion/collection/entry
         guard
         rebellion/private/static-name
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

;@------------------------------------------------------------------------------

(define (in-multiset set)
  (for*/stream ([(v count) (in-immutable-hash (multiset-frequencies set))]
                [_ (in-range count)])
    v))

(define (make-multiset-props descriptor)
  (define name (record-type-name (record-descriptor-type descriptor)))
  (define equal+hash (default-record-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this) (in-multiset this))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)
        (cons prop:sequence in-multiset)))

(define-record-type multiset (size frequencies unique-elements)
  #:property-maker make-multiset-props
  #:omit-root-binding)

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

(define/guard (multiset-remove set element #:copies [copies 1])
  (guard (multiset-contains? set element) #:else
    set)
  (guard (not (equal? copies +inf.0)) #:else
    (multiset-set-frequency set element 0))
  (define frequency (max (- (multiset-frequency set element) copies) 0))
  (multiset-set-frequency set element frequency))

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


(module+ test
  (test-case (name-string multiset-add-all)
    (check-equal? (multiset-add-all (multiset 1 1 2 3) (in-range 0 5))
                  (multiset 0 1 1 1 2 2 3 3 4))
    (check-equal? (multiset-add-all (multiset 1 2 3) (multiset 1 2 3))
                  (multiset 1 1 2 2 3 3))))


(define (multiset-subtract a b)
  (for/fold ([size (multiset-size a)]
             [frequencies (multiset-frequencies a)]
             [unique-elements (multiset-unique-elements a)]
             #:result
             (constructor:multiset
              #:size size
              #:frequencies frequencies
              #:unique-elements unique-elements))
            ([(v removal-count) (in-hash (multiset-frequencies b))])
    (cond
      [(hash-has-key? frequencies v)
       (define existing-count (hash-ref frequencies v))
       (if (> existing-count removal-count)
           (values (- size removal-count)
                   (hash-set frequencies v (- existing-count removal-count))
                   unique-elements)
           (values (- size existing-count)
                   (hash-remove frequencies v)
                   (set-remove unique-elements v)))]
      [else (values size frequencies unique-elements)])))


(define (multiset-remove-all set seq)
  (if (multiset? seq)
      (multiset-subtract set seq)
      (for/fold ([set set])
                ([v seq])
        (multiset-remove set v))))


(module+ test
  (test-case (name-string multiset-remove-all)
    (check-equal? (multiset-remove-all (multiset 1 1 1 2 3 3) (in-range 0 10)) (multiset 1 1 3))
    (check-equal?
     (multiset-remove-all (multiset-of-frequencies (entry 'a 10) (entry 'b 6))
                          (multiset-of-frequencies (entry 'a 3) (entry 'b 100) (entry 'c 5)))
     (multiset-of-frequencies (entry 'a 7)))))


(define (multiset-of-frequencies . freqs)
  (for/fold ([size 0]
             [freq-hash empty-hash]
             [unique-elems empty-set]
             #:result (constructor:multiset #:size size
                                            #:frequencies freq-hash
                                            #:unique-elements unique-elems))
            ([f (in-list freqs)])
    (define v (entry-key f))
    (define count (entry-value f))
    (when (set-member? unique-elems v)
      (raise-arguments-error
       (name multiset-of-frequencies)
       "duplicate frequency entries for the same element are not allowed"
       "initial entry" (entry v (hash-ref freq-hash v))
       "duplicate entry" f))
    (values (+ size count)
            (hash-set freq-hash v count)
            (set-add unique-elems v))))


(module+ test
  (test-case (name-string multiset-of-frequencies)
    (check-equal? (multiset-of-frequencies) (multiset))
    (check-equal? (multiset-of-frequencies (entry 'a 2) (entry 'b 3) (entry 'c 1))
                  (multiset 'a 'a 'b 'b 'b 'c))
    (define (call-with-duplicates)
      (multiset-of-frequencies (entry 'a 1) (entry 'a 2)))
    (check-exn exn:fail:contract? call-with-duplicates)
    (check-exn #rx"duplicate frequency entries" call-with-duplicates)))


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


(module+ test
  (test-case (name-string multiset-contains?)
    (define letters (multiset 'a 'b 'b 'b 'c 'c 'd))
    (check-true (multiset-contains? letters 'a))
    (check-true (multiset-contains? letters 'b))
    (check-false (multiset-contains? letters 'foo))))


(define (multiset-contains-all? set seq)
  (if (multiset? seq)
      (subset? (multiset-unique-elements seq) (multiset-unique-elements set))
      (for/and ([v seq])
        (set-member? (multiset-unique-elements set) v))))


(module+ test
  (test-case (name-string multiset-contains-all?)
    (define letters (multiset 'a 'b 'b 'b 'c 'c 'd))
    (check-true (multiset-contains-all? letters (list)))
    (check-true (multiset-contains-all? letters (list 'a 'b)))
    (check-true (multiset-contains-all? letters (list 'a 'a 'a 'a 'a)))
    (check-false (multiset-contains-all? letters (list 'c 'd 'e 'f)))
    (check-false (multiset-contains-all? letters (list 'e 'e 'e)))
    (check-true (multiset-contains-all? letters (multiset 'a 'b)))
    (check-true (multiset-contains-all? letters (multiset 'a 'a 'a 'a 'a)))
    (check-false (multiset-contains-all? letters (multiset 'c 'd 'e 'f)))
    (check-false (multiset-contains-all? letters (multiset 'e 'e 'e)))))


(define (multiset-contains-any? set seq)
  (let* ([seq (if (multiset? seq) (multiset-unique-elements seq) seq)])
    (for/or ([v seq])
      (multiset-contains? set v))))


(module+ test
  (test-case (name-string multiset-contains-any?)
    (define letters (multiset 'a 'b 'b 'b 'c 'c 'd))
    (check-false (multiset-contains-any? letters (list)))
    (check-true (multiset-contains-any? letters (list 'a 'b)))
    (check-true (multiset-contains-any? letters (list 'a 'a 'a 'a 'a)))
    (check-true (multiset-contains-any? letters (list 'c 'd 'e 'f)))
    (check-false (multiset-contains-any? letters (list 'e 'e 'e)))
    (check-true (multiset-contains-any? letters (multiset 'a 'b)))
    (check-true (multiset-contains-any? letters (multiset 'a 'a 'a 'a 'a)))
    (check-true (multiset-contains-any? letters (multiset 'c 'd 'e 'f)))
    (check-false (multiset-contains-any? letters (multiset 'e 'e 'e)))))


(define (multiset-contains-none? set seq)
  (not (multiset-contains-any? set seq)))


(module+ test
  (test-case (name-string multiset-contains-none?)
    (define letters (multiset 'a 'b 'b 'b 'c 'c 'd))
    (check-true (multiset-contains-none? letters (list)))
    (check-false (multiset-contains-none? letters (list 'a 'b)))
    (check-false (multiset-contains-none? letters (list 'a 'a 'a 'a 'a)))
    (check-false (multiset-contains-none? letters (list 'c 'd 'e 'f)))
    (check-true (multiset-contains-none? letters (list 'e 'e 'e)))
    (check-false (multiset-contains-none? letters (multiset 'a 'b)))
    (check-false (multiset-contains-none? letters (multiset 'a 'a 'a 'a 'a)))
    (check-false (multiset-contains-none? letters (multiset 'c 'd 'e 'f)))
    (check-true (multiset-contains-none? letters (multiset 'e 'e 'e)))))


(define (frequency-hash->list frequencies)
  (for*/list ([(elem freq) (in-hash frequencies)]
              [_ (in-range freq)])
    elem))

(module+ test
  (test-case "queries"
    (define letters (multiset 'a 'b 'b 'b 'c 'c 'd))
    (check-equal? (multiset-frequencies letters) (hash 'a 1 'b 3 'c 2 'd 1))
    (check-equal? (multiset-frequency letters 'b) 3)
    (check-equal? (multiset-frequency letters 'foo) 0)
    (check-equal? (multiset-size letters) 7)
    (check-equal? (multiset-unique-elements letters) (set 'a 'b 'c 'd)))
  (test-case "conversion"
    (check-equal? (sequence->multiset "hello")
                  (multiset #\h #\e #\l #\l #\o)))
  (test-case "iteration"
    (check-equal? (for/multiset ([char (in-string "hello")]) char)
                  (multiset #\h #\e #\l #\l #\o))
    (check-equal? (for/set ([letter (in-multiset (multiset 'a 'b 'b 'b 'c 'c 'd))]) letter)
                  (set 'a 'b 'c 'd))))
