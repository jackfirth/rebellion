#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [vector-builder? predicate/c]
  [unused-vector-builder/c predicate/c]
  [make-vector-builder
   (->* () ((sequence/c any/c) #:expected-size (or/c natural? #f)) vector-builder?)]
  [vector-builder-add (-> vector-builder? any/c ... vector-builder?)]
  [vector-builder-add-all (-> vector-builder? (sequence/c any/c) vector-builder?)]
  [build-vector (-> vector-builder? immutable-vector?)]
  [build-mutable-vector (-> vector-builder? (and/c vector? (not/c immutable?)))]))


(require racket/contract/combinator
         racket/math
         racket/sequence
         racket/vector
         rebellion/collection/immutable-vector
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/private/static-name)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct vector-builder
  ([contents #:mutable]
   [position #:mutable])
  #:constructor-name constructor:vector-builder)


(define (make-vector-builder [initial-contents empty-list]
                             #:expected-size [expected-size* #f])
  (define expected-size (or expected-size* 16))
  (define initial-position (sequence-length initial-contents))
  (define initial-size (max initial-position expected-size))
  (define contents (make-vector initial-size))
  (for ([v initial-contents] [i (in-naturals)]) (vector-set! contents i v))
  (constructor:vector-builder contents initial-position))


(define (vector-builder-add builder . vs)
  (define len (length vs))
  (define position (vector-builder-position builder))
  (define new-position (+ position len))
  (define capacity (vector-length (vector-builder-contents builder)))
  (when (> new-position capacity)
    (define extended-contents (make-vector (* capacity 2)))
    (vector-copy! extended-contents 0 (vector-builder-contents builder))
    (set-vector-builder-contents! builder extended-contents))
  (define contents (vector-builder-contents builder))
  (for ([v (in-list vs)] [i (in-naturals position)])
    (vector-set! contents i v))
  (set-vector-builder-position! builder new-position)
  builder)


(define (vector-builder-add-all builder seq)
  (define len (sequence-length seq))
  (define position (vector-builder-position builder))
  (define new-position (+ position len))
  (define capacity (vector-length (vector-builder-contents builder)))
  (when (> new-position capacity)
    (define extended-contents (make-vector (* capacity 2)))
    (vector-copy! extended-contents 0 (vector-builder-contents builder))
    (set-vector-builder-contents! builder extended-contents))
  (define contents (vector-builder-contents builder))
  (for ([v seq] [i (in-naturals position)])
    (vector-set! contents i v))
  (set-vector-builder-position! builder new-position)
  builder)


(define (build-vector builder)
  (define contents (vector-builder-contents builder))
  (define position (vector-builder-position builder))
  (vector->immutable-vector (vector-copy contents 0 position)))

(define (build-mutable-vector builder)
  (define contents (vector-builder-contents builder))
  (define position (vector-builder-position builder))
  (if (equal? (vector-length contents) position)
      contents
      (vector-copy contents 0 position)))


(module+ test
  (test-case "vector builders should support basic folding"
    (define vec
      (build-vector
       (foldl (λ (v builder) (vector-builder-add builder v))
              (make-vector-builder)
              (list 1 2 3 4 5))))
    (check-true (immutable? vec))
    (check-equal? vec (vector-immutable 1 2 3 4 5)))
  

  (test-case "vector-builder-add"
    (define builder (make-vector-builder))
    (vector-builder-add builder 1 2 3)
    (vector-builder-add builder 4 5 6)
    (define vec (build-vector builder))
    (check-equal? vec (vector-immutable 1 2 3 4 5 6)))

  (test-case "vector-builder-add-all"
    (define builder (make-vector-builder))
    (vector-builder-add-all builder (in-range 0 5))
    (vector-builder-add-all builder (in-range 5 10))
    (define vec (build-vector builder))
    (check-equal? vec (vector-immutable 0 1 2 3 4 5 6 7 8 9)))

  (test-case "vector builders should allow multiple usages"
    (define builder (make-vector-builder))
    (check-equal? (vector-builder-add builder 1) (vector-immutable 1))
    (check-equal? (vector-builder-add builder 2) (vector-immutable 1 2))))


;@----------------------------------------------------------------------------------------------------
;; Deprecated APIs


;; Vector builders used to be single-use only. They're not anymore, so now this contract does nothing
;; different from vector-builder?
(define unused-vector-builder/c vector-builder?)
