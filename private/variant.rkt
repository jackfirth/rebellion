#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [variant (unconstrained-domain-> variant?)]
  [variant? (-> any/c boolean?)]
  [variant-value (-> variant? any/c)]
  [variant-tag (-> variant? keyword?)]
  [variant-tagged-as? (-> variant? keyword? boolean?)]))

(require racket/list
         racket/struct
         rebellion/base/generative-token
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (make-variant-properties descriptor)
  (define type-name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define custom-write
    (make-constructor-style-printer
      (λ (this) type-name)
      (λ (this)
        (define tag (accessor this 0))
        (define value (accessor this 1))
        (define tag-str (string-append "#:" (keyword->string tag)))
        (list (unquoted-printing-string tag-str) value))))
  (list (cons prop:custom-write custom-write)
        (cons prop:equal+hash (default-tuple-equal+hash descriptor))))

(define-tuple-type variant (tag value)
  #:omit-root-binding
  #:property-maker make-variant-properties)

(define (variant-keyword-function kws kw-args)
  (when (> (length kws) 1)
    (raise-arguments-error 'variant
                           "multiple keyword arguments"
                           "keywords" kws
                           "values" kw-args))
  (when (< (length kws) 1)
    (raise-arguments-error 'variant "no arguments given"))
  (constructor:variant (first kws) (first kw-args)))

(define variant
  (procedure-reduce-keyword-arity
   (make-keyword-procedure variant-keyword-function)
   0
   empty
   #f))

(module+ test
  (define var (variant #:success 42))
  (check-equal? (variant-tag var) '#:success)
  (check-equal? (variant-value var) 42)
  (check-equal? var (variant #:success 42)))

(define (variant-tagged-as? var tag-keyword)
  (equal? (variant-tag var) tag-keyword))

(module+ test
  (test-case "variant-tagged-as?"
    (check-true (variant-tagged-as? (variant #:success 42) '#:success))
    (check-false (variant-tagged-as? (variant #:failure "oh no") '#:success))))
