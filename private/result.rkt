#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [result? predicate/c]
  [success (-> any/c success?)]
  [success? predicate/c]
  [success-value (-> success? any/c)]
  [failure? predicate/c]
  [failure (-> any/c failure?)]
  [failure-error (-> failure? any/c)]
  [result-case
   (-> result? #:success (-> any/c any/c) #:failure (-> any/c any/c) any/c)]))

(require rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type success (value))
(define-tuple-type failure (error))

(define (result? v) (or (success? v) (failure? v)))

(define (result-case result #:success success-handler #:failure failure-handler)
  (if (success? result)
      (success-handler (success-value result))
      (failure-handler (failure-error result))))
