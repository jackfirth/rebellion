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

(define into-hash
  (make-fold-reducer hash-set-entry empty-hash #:name 'into-hash))

(define into-mutable-hash
  (make-effectful-fold-reducer (λ (h e) (hash-set-entry! h e) h)
                               make-hash
                               values
                               #:name 'into-mutable-hash))

(define (cons-pair->entry p) (entry (car p) (cdr p)))

(define (in-hash-entries h)
  (sequence-map (in-immutable-hash-pairs h) cons-pair->entry))

(define (in-mutable-hash-entries h)
  (sequence-map (in-mutable-hash-pairs h) cons-pair->entry))
