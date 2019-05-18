#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [immutable-bytes? predicate/c]))

;@------------------------------------------------------------------------------

(define (immutable-bytes? v) (and (bytes? v) (immutable? v)))
