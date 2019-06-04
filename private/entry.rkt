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

(define entry-descriptor (tuple-type-make-implementation (tuple-type 'entry 2)))
(define entry (tuple-descriptor-constructor entry-descriptor))
(define entry? (tuple-descriptor-predicate entry-descriptor))
(define entry-key (make-tuple-field-accessor entry-descriptor 0 'key))
(define entry-value (make-tuple-field-accessor entry-descriptor 1 'value))
