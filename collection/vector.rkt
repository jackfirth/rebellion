#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [mutable-vector? (-> any/c boolean?)]
  [into-vector
   (->* () (#:size (or/c natural? +inf.0)) (reducer/c any/c immutable-vector?))]
  [into-mutable-vector
   (->* () (#:size (or/c natural? +inf.0)) (reducer/c any/c mutable-vector?))]
  [sequence->vector
   (-> (or/c vector? list? set? (sequence/c any/c))
       immutable-vector?)]))

(require guard
         racket/math
         racket/sequence
         racket/set
         rebellion/collection/immutable-vector
         rebellion/collection/vector/builder
         rebellion/private/static-name
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (mutable-vector? v) (and (vector? v) (not (immutable? v))))

(define into-unlimited-vector
  (make-effectful-fold-reducer vector-builder-add
                               make-vector-builder
                               build-vector
                               #:name (name into-vector)))

(define into-unlimited-mutable-vector
  (make-effectful-fold-reducer vector-builder-add
                               make-vector-builder
                               build-mutable-vector
                               #:name (name into-mutable-vector)))

(define/guard (into-vector #:size [size +inf.0])
  (guard (not (equal? size +inf.0)) #:else
    into-unlimited-vector)
  (define (make-builder) (make-vector-builder #:expected-size size))
  (define unlimited
    (make-effectful-fold-reducer
     vector-builder-add make-builder build-vector #:name (name into-vector)))
  (reducer-limit unlimited size))

(define/guard (into-mutable-vector #:size [size +inf.0])
  (guard (not (equal? size +inf.0)) #:else
    into-unlimited-mutable-vector)
  (define (make-builder) (make-vector-builder #:expected-size size))
  (define unlimited
    (make-effectful-fold-reducer
     vector-builder-add make-builder build-mutable-vector
     #:name (name into-mutable-vector)))
  (reducer-limit unlimited size))

(define/guard (sequence->vector seq)
  (guard (not (vector? seq)) #:else
    (if (immutable? seq) seq (vector->immutable-vector seq)))
  (cond
    [(list? seq) (vector->immutable-vector (list->vector seq))]
    [(set? seq) (vector->immutable-vector (for/vector #:length (set-count seq) ([v seq]) v))]
    [else (vector->immutable-vector (for/vector ([v seq]) v))]))

(module+ test
  (test-case "into-vector"
    (check-equal? (reduce-all (into-vector) (in-range 5))
                  (vector-immutable 0 1 2 3 4))
    (check-equal? (reduce-all (into-vector #:size 3) (in-range 5))
                  (vector-immutable 0 1 2))
    (check-equal? (reduce-all (into-vector #:size 10) (in-range 5))
                  (vector-immutable 0 1 2 3 4)))

  (test-case "into-mutable-vector"
    (check-equal? (reduce-all (into-mutable-vector) (in-range 5))
                  (vector 0 1 2 3 4))
    (check-equal? (reduce-all (into-mutable-vector #:size 3) (in-range 5))
                  (vector 0 1 2))
    (check-equal? (reduce-all (into-mutable-vector #:size 10) (in-range 5))
                  (vector 0 1 2 3 4)))

  (test-case "sequence->vector"
    (check-equal? (sequence->vector (vector-immutable 0 1 2 3 4))
                  (vector-immutable 0 1 2 3 4))
    (check-equal? (sequence->vector (vector 0 1 2 3 4))
                  (vector-immutable 0 1 2 3 4))
    (check-equal? (sequence->vector (list 0 1 2 3 4))
                  (vector-immutable 0 1 2 3 4))
    (check-equal? (sequence->vector (in-range 0 5))
                  (vector-immutable 0 1 2 3 4))))
