#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [into-vector (->* () (#:size (or/c natural? +inf.0)) reducer?)]
  [into-mutable-vector (->* () (#:size (or/c natural? +inf.0)) reducer?)]
  [sequence->vector
   (-> (or/c vector? list? set? (sequence/c any/c))
       (and/c vector? immutable?))]))

(require racket/math
         racket/sequence
         racket/set
         rebellion/collection/vector/builder
         rebellion/private/static-name
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

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

(define/name (into-vector #:size [size +inf.0])
  (cond
    [(equal? size +inf.0) into-unlimited-vector]
    [else
     (define (make-builder) (make-vector-builder #:expected-size size))
     (define unlimited
       (make-effectful-fold-reducer vector-builder-add
                                    make-builder
                                    build-vector
                                    #:name enclosing-function-name))
     (reducer-limit unlimited size)]))

(define/name (into-mutable-vector #:size [size +inf.0])
  (cond
    [(equal? size +inf.0) into-unlimited-mutable-vector]
    [else
     (define (make-builder) (make-vector-builder #:expected-size size))
     (define unlimited
       (make-effectful-fold-reducer vector-builder-add
                                    make-builder
                                    build-mutable-vector
                                    #:name enclosing-function-name))
     (reducer-limit unlimited size)]))

(define (sequence->vector seq)
  (cond
    [(vector? seq)
     (cond
       [(immutable? seq) seq]
       [else (vector->immutable-vector seq)])]
    [(list? seq)
     (vector->immutable-vector (list->vector seq))]
    [(set? seq)
     (vector->immutable-vector
      (for/vector #:length (set-count seq) ([v seq]) v))]
    [(vector->immutable-vector (for/vector ([v seq]) v))]))

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
