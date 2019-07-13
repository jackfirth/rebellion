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
         rebellion/base/generative-token)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define variant-datatype-token (make-generative-token))

(struct variant (tag value)
  #:constructor-name plain-variant
  #:omit-define-syntaxes

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (this) 'variant)
      (λ (this)
        (define tag (variant-tag this))
        (define tag-str (string-append "#:" (keyword->string tag)))
        (list (unquoted-printing-string tag-str) (variant-value this)))))]

  #:methods gen:equal+hash
  [(define (equal-proc this other recur)
     (and (recur (variant-tag this) (variant-tag other))
          (recur (variant-value this) (variant-value other))))
   (define (hash-proc this recur)
     (recur
         (list variant-datatype-token (variant-tag this) (variant-value this))))
   (define hash2-proc hash-proc)])

(define (variant-keyword-function kws kw-args)
  (when (> (length kws) 1)
    (raise-arguments-error 'variant
                           "multiple keyword arguments"
                           "keywords" kws
                           "values" kw-args))
  (when (< (length kws) 1)
    (raise-arguments-error 'variant "no arguments given"))
  (plain-variant (first kws) (first kw-args)))

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
