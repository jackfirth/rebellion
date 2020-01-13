#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [immutable-hash? predicate/c]
  [mutable-hash? predicate/c]
  [empty-hash empty-immutable-hash?]
  [empty-immutable-hash? predicate/c]
  [nonempty-immutable-hash? predicate/c]
  [in-hash-entries (-> immutable-hash? (sequence/c entry?))]
  [in-mutable-hash-entries (-> mutable-hash? (sequence/c entry?))]
  [into-hash reducer?]
  [into-mutable-hash reducer?]
  [combine-into-hash (-> (-> any/c any/c any/c) reducer?)]
  [combine-into-mutable-hash (-> (-> any/c any/c any/c) reducer?)]
  [hash-set-entry (-> immutable-hash? entry? immutable-hash?)]
  [hash-set-entry! (-> mutable-hash? entry? void?)]
  [hash-key-set (-> immutable-hash? set?)]
  [hash-ref-safe (-> immutable-hash? any/c option?)]))

(require racket/sequence
         racket/set
         racket/splicing
         rebellion/base/option
         rebellion/collection/entry
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define empty-hash (hash))
(define (mutable-hash? v) (and (hash? v) (not (immutable? v))))
(define (immutable-hash? v) (and (hash? v) (immutable? v)))
(define (hash-set-entry h e) (hash-set h (entry-key e) (entry-value e)))
(define (hash-set-entry! h e) (hash-set! h (entry-key e) (entry-value e)))

(define (empty-immutable-hash? v)
  (and (hash? v) (immutable? v) (hash-empty? v)))

(define (nonempty-immutable-hash? v)
  (and (hash? v) (immutable? v) (not (hash-empty? v))))

(define (check-not-duplicate-key who h e)
  (define k (entry-key e))
  (when (hash-has-key? h k)
    (raise-arguments-error who
                           "duplicate key"
                           "key" k
                           "existing value" (hash-ref h k)
                           "new value" (entry-value e))))

(splicing-local
    [(define (checked-set h e)
       (check-not-duplicate-key 'into-hash h e)
       (hash-set-entry h e))]
  (define into-hash
    (make-fold-reducer checked-set empty-hash #:name 'into-hash)))

(splicing-local
    [(define (checked-set h e)
       (check-not-duplicate-key 'into-mutable-hash h e)
       (hash-set-entry! h e)
       h)]
  (define into-mutable-hash
    (make-effectful-fold-reducer checked-set make-hash values
                                 #:name 'into-mutable-hash)))

(define (combine-into-hash combiner)
  (define (add-entry h e)
    (define k (entry-key e))
    (define v (entry-value e))
    (if (hash-has-key? h k)
        (hash-update h k (λ (previous) (combiner previous v)))
        (hash-set h k v)))
  (make-fold-reducer add-entry empty-hash #:name 'combine-into-hash))

(define (combine-into-mutable-hash combiner)
  (define (add-entry h e)
    (define k (entry-key e))
    (define v (entry-value e))
    (if (hash-has-key? h k)
        (hash-update! h k (λ (previous) (combiner previous v)))
        (hash-set! h k v))
    h)
  (make-effectful-fold-reducer add-entry make-hash values
                               #:name 'combine-into-mutable-hash))

(define (cons-pair->entry p) (entry (car p) (cdr p)))

(define (in-hash-entries h)
  (sequence-map cons-pair->entry (in-immutable-hash-pairs h)))

(define (in-mutable-hash-entries h)
  (sequence-map cons-pair->entry (in-mutable-hash-pairs h)))

(define (hash-key-set h) (for/set ([k (in-immutable-hash-keys h)]) k))

(define (hash-ref-safe h k)
  (if (hash-has-key? h k) (present (hash-ref h k)) absent))

(module+ test
  (test-case "into-hash"
    (check-equal? (reduce into-hash (entry 'a 1) (entry 'b 2) (entry 'c 3))
                  (hash 'a 1 'b 2 'c 3))
    (check-exn exn:fail:contract?
               (λ () (reduce into-hash (entry 'a 1) (entry 'a 2))))
    (check-equal? (reduce into-hash) empty-hash))
  
  (test-case "into-mutable-hash"
    (define h (reduce into-mutable-hash (entry 'a 1) (entry 'b 2) (entry 'c 3)))
    (check-pred mutable-hash? h)
    (check-exn exn:fail:contract?
               (λ () (reduce into-mutable-hash (entry 'a 1) (entry 'a 2)))))
  
  (test-case "combine-into-hash"
    (check-equal? (reduce (combine-into-hash string-append)
                          (entry 'a "foo")
                          (entry 'b "bar")
                          (entry 'a "baz")
                          (entry 'a "blah"))
                  (hash 'a "foobazblah"
                        'b "bar")))
  
  (test-case "combine-into-mutable-hash"
    (define h
      (reduce (combine-into-mutable-hash string-append)
              (entry 'a "foo")
              (entry 'b "bar")
              (entry 'a "baz")
              (entry 'a "blah")))
    (check-pred mutable-hash? h))
  
  (test-case "in-hash-entries"
    (check-equal? (reduce-all into-hash (in-hash-entries (hash 'a 1 'b 2)))
                  (hash 'a 1 'b 2)))
  
  (test-case "in-mutable-hash-entries"
    (define h (make-hash (list (cons 'a 1) (cons 'b 2))))
    (check-equal? (reduce-all into-hash (in-mutable-hash-entries h))
                  (hash 'a 1 'b 2)))
  
  (test-case "hash-key-set"
    (check-equal? (hash-key-set (hash 'a 1 'b 2)) (set 'a 'b)))

  (test-case "hash-ref-safe"
    (check-equal? (hash-ref-safe (hash 'a 1 'b 2) 'a) (present 1))
    (check-equal? (hash-ref-safe (hash 'a 1 'b 2) 'b) (present 2))
    (check-equal? (hash-ref-safe (hash 'a 1 'b 2) 'c) absent)))
