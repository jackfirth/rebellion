#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [filter? (-> any/c boolean?)]
  [keep? (-> any/c boolean?)]
  [discard? (-> any/c boolean?)]
  [keep keep?]
  [discard discard?]
  [keep-when (-> (-> any/c boolean?) filter?)]
  [keep-unless (-> (-> any/c boolean?) filter?)]
  [discard-when (-> (-> any/c boolean?) filter?)]
  [discard-unless (-> (-> any/c boolean?) filter?)]
  [make-filter (-> (-> any/c (or/c keep? discard?)) filter?)]
  [filter-apply (-> filter? any/c (or/c keep? discard?))]
  [filter-function (-> filter? (-> any/c (or/c keep? discard?)))]))

(require rebellion/singleton)

;@------------------------------------------------------------------------------

(define-singleton-type keep)
(define-singleton-type discard)

(struct filter (function)
  #:constructor-name make-filter
  #:property prop:procedure (struct-field-index function))

(define (keep-when predicate)
  (make-filter (λ (v) (if (predicate v) keep discard))))

(define (keep-unless predicate)
  (make-filter (λ (v) (if (predicate v) discard keep))))

(define (discard-when predicate)
  (make-filter (λ (v) (if (predicate v) discard keep))))

(define (discard-unless predicate)
  (make-filter (λ (v) (if (predicate v) keep discard))))

(define (filter-apply filt v) ((filter-function filt) v))
