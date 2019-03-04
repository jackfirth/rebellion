#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [pair (-> any/c any/c any/c)]
  [pair? (-> any/c boolean?)]
  [pair-first (-> pair? any/c)]
  [pair-second (-> pair? any/c)]))

(require rebellion/tuple-type)

;@------------------------------------------------------------------------------

(define pair-descriptor (tuple-type-make-implementation (tuple-type 'pair 2)))
(define pair (tuple-descriptor-constructor pair-descriptor))
(define pair? (tuple-descriptor-predicate pair-descriptor))
(define pair-first (make-tuple-field-accessor pair-descriptor 0 'first))
(define pair-second (make-tuple-field-accessor pair-descriptor 1 'second))
