#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [entry (-> any/c any/c entry?)]
  [entry? (-> any/c boolean?)]
  [entry-key (-> entry? any/c)]
  [entry-value (-> entry? any/c)]))

(require rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type entry (key value))
