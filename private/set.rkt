#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [into-set reducer?]
  [into-mutable-set reducer?]))

(require racket/set
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define empty-set (set))

(define (mutable-set? v) (set-mutable? v))

(define into-set (make-fold-reducer set-add empty-set #:name 'into-set))

(define into-mutable-set
  (make-effectful-fold-reducer (λ (st element) (set-add! st element) st)
                               mutable-set
                               values
                               #:name 'into-mutable-set))

(module+ test
  (test-case "into-set"
    (check-equal? (reduce into-set 1 4 2 3 4 2) (set 1 2 3 4))
    (check-equal? (reduce into-set) empty-set)
    (check-equal? (reduce into-set 1 1 1 1 1 1) (set 1)))
  (test-case "into-mutable-set"
    (define st (reduce into-mutable-set 1 2 3))
    (check-pred mutable-set? st)))