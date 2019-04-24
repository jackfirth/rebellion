#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [json? predicate/c]
  [json-object? predicate/c]
  [json-array? predicate/c]
  [json-string? predicate/c]
  [json-number? predicate/c]
  [json-true json-true?]
  [json-true? predicate/c]
  [json-false json-false?]
  [json-false? predicate/c]
  [json-boolean? predicate/c]
  [json-null? predicate/c]
  [json-type (-> json? json-type?)]
  [json-type? predicate/c]
  [json-number-type json-type?]
  [json-string-type json-type?]
  [json-boolean-type json-type?]
  [json-null-type json-type?]
  [json-true-type json-type?]
  [json-false-type json-type?]
  [json-object-type json-type?]
  [json-array-type json-type?]
  [json-case (-> json?
                 #:number (-> json-number? any/c)
                 #:string (-> json-string? any/c)
                 #:boolean (-> json-boolean? any/c)
                 #:null (-> json-null? any/c)
                 #:object (-> json-object? any/c)
                 #:array (-> json-array? any/c)
                 any/c)]))

(require rebellion/singleton)

;@------------------------------------------------------------------------------

(define-singleton-type json-true)
(define-singleton-type json-false)
(define-singleton-type json-null)

(define-tuple-type json-number (integer-part fraction-part exponent-part))

(define (json? v)
  (or (json-true? v)
      (json-false? v)
      (json-null? v)
      (json-number? v)
      (json-string? v)
      (json-object? v)
      (json-array? v)))
