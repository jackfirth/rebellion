#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [immutable-set/c (-> chaperone-contract? contract?)]
  [mutable-set/c (-> contract? contract?)]
  [empty-set set?]))

;@------------------------------------------------------------------------------

(require racket/set)

(define empty-set (set))

(define (immutable-set/c element-contract)
  (set/c element-contract #:cmp 'equal #:kind 'immutable))

(define (mutable-set/c element-contract)
  (set/c element-contract #:cmp 'equal #:kind 'mutable))
