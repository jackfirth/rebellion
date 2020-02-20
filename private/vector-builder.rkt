#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [vector-builder? predicate/c]
  [unused-vector-builder/c flat-contract?]
  [make-vector-builder
   (->* () ((sequence/c any/c) #:expected-size (or/c natural? #f))
        unused-vector-builder/c)]
  [vector-builder-add
   (-> unused-vector-builder/c any/c ... unused-vector-builder/c)]
  [vector-builder-add-all
   (-> unused-vector-builder/c (sequence/c any/c) unused-vector-builder/c)]
  [build-vector (-> unused-vector-builder/c immutable-vector?)]
  [build-mutable-vector
   (-> unused-vector-builder/c (and/c vector? (not/c immutable?)))]))

(require racket/contract/combinator
         racket/math
         racket/sequence
         racket/vector
         rebellion/collection/immutable-vector
         rebellion/collection/list
         rebellion/private/static-name)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(struct vector-builder
  ([uses-remaining #:mutable]
   next-total-uses
   [contents #:mutable]
   [position #:mutable])
  #:constructor-name constructor:vector-builder)

(define/name unused-vector-builder/c
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(not (vector-builder? v))
        (define template '(expected: "vector-builder?" given: "~e"))
        (λ (blame) (raise-blame-error blame v template v))]
       [(not (positive? (vector-builder-uses-remaining v)))
        (define template
          '("this builder has already been used\n  builder: ~e"))
        (λ (blame) (raise-blame-error blame v template v))]
       [else #t]))
   #:name enclosing-variable-name))

(define (make-vector-builder [initial-contents empty-list]
                             #:expected-size [expected-size* #f])
  (define expected-size (or expected-size* 16))
  (define initial-position (sequence-length initial-contents))
  (define initial-size (max initial-position expected-size))
  (define contents (make-vector initial-size))
  (for ([v initial-contents] [i (in-naturals)]) (vector-set! contents i v))
  (constructor:vector-builder 1 2 contents initial-position))

(define (vector-builder-mark-used-once builder)
  (define new-uses (sub1 (vector-builder-uses-remaining builder)))
  (set-vector-builder-uses-remaining! builder new-uses)
  (cond
    [(zero? new-uses)
     (define next-total (vector-builder-next-total-uses builder))
     (define contents (vector-builder-contents builder))
     (define position (vector-builder-position builder))
     (constructor:vector-builder next-total (* next-total 2) contents position)]
    [else builder]))

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
  (vector-builder-mark-used-once builder))

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
  (vector-builder-mark-used-once builder))

(define (build-vector builder)
  (set-vector-builder-uses-remaining! builder 0)
  (define contents (vector-builder-contents builder))
  (define position (vector-builder-position builder))
  (vector->immutable-vector (vector-copy contents 0 position)))

(define (build-mutable-vector builder)
  (set-vector-builder-uses-remaining! builder 0)
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
  
  (test-case "vector builders should raise errors when used after building"
    (define builder
      (foldl (λ (v builder) (vector-builder-add builder v))
             (make-vector-builder)
             (list 1 2 3 4 5)))
    (define vec (build-vector builder))
    (check-exn #rx"this builder has already been used"
               (λ () (build-vector builder)))
    (check-exn #rx"this builder has already been used"
               (λ () (vector-builder-add builder 1)))
    (check-exn #rx"this builder has already been used"
               (λ () (vector-builder-add-all builder (list 1 2 3))))
    (check-exn #rx"this builder has already been used"
               (λ () (build-mutable-vector builder))))

  (test-case "vector-builder-add"
    (define builder0 (make-vector-builder))
    (define builder1 (vector-builder-add builder0 1 2 3))
    (define builder2 (vector-builder-add builder1 4 5 6))
    (define vec (build-vector builder2))
    (check-equal? vec (vector-immutable 1 2 3 4 5 6)))

  (test-case "vector-builder-add-all"
    (define builder0 (make-vector-builder))
    (define builder1 (vector-builder-add-all builder0 (in-range 0 5)))
    (define builder2 (vector-builder-add-all builder1 (in-range 5 10)))
    (define vec (build-vector builder2))
    (check-equal? vec (vector-immutable 0 1 2 3 4 5 6 7 8 9)))

  (test-case "vector builders should not allow multiple usages"
    (define builder (make-vector-builder))
    (vector-builder-add builder 42)
    (check-exn #rx"this builder has already been used"
               (λ () (vector-builder-add builder 42)))))
