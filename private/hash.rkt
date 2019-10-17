#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [immutable-hash? predicate/c]
  [mutable-hash? predicate/c]
  [empty-hash immutable-hash?]
  [in-hash-entries (-> immutable-hash? (sequence/c entry?))]
  [in-mutable-hash-entries (-> mutable-hash? (sequence/c entry?))]
  [into-hash reducer?]
  [into-mutable-hash reducer?]
  [hash-set-entry (-> immutable-hash? entry? immutable-hash?)]
  [hash-set-entry! (-> mutable-hash? entry? void?)]))

(require racket/sequence
         racket/splicing
         rebellion/streaming/reducer
         rebellion/collection/entry)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define empty-hash (hash))
(define (mutable-hash? v) (and (hash? v) (not (immutable? v))))
(define (immutable-hash? v) (and (hash? v) (immutable? v)))
(define (hash-set-entry h e) (hash-set h (entry-key e) (entry-value e)))
(define (hash-set-entry! h e) (hash-set! h (entry-key e) (entry-value e)))

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
  
(define (cons-pair->entry p) (entry (car p) (cdr p)))

(define (in-hash-entries h)
  (sequence-map (in-immutable-hash-pairs h) cons-pair->entry))

(define (in-mutable-hash-entries h)
  (sequence-map (in-mutable-hash-pairs h) cons-pair->entry))

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
               (λ () (reduce into-mutable-hash (entry 'a 1) (entry 'a 2))))))
