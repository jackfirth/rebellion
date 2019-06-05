#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [pair (-> any/c any/c any/c)]
  [pair? (-> any/c boolean?)]
  [pair-first (-> pair? any/c)]
  [pair-second (-> pair? any/c)]))

(require rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type pair (first second))
