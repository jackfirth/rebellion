#lang racket/base

(provide strict-cond)

(require (for-syntax racket/base)
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define-syntax-parser strict-cond
  #:literals (else)
  #:track-literals
  [(_ [condition:expr body ...+] ... [else ~! else-body ...+])
   #'(cond [condition body ...] ... [else else-body ...])]
  [(_ [condition:expr body ...+] ...)
   #:with raise-error
   (syntax/loc this-syntax
     (raise-arguments-error 'strict-cond "none of the conditions were true"))
   #'(cond [condition body ...] ... [else raise-error])])

(module+ test
  (test-case (name-string strict-cond)
    (check-equal? (strict-cond [else 42]) 42)
    (check-equal? (strict-cond [#t 1] [else 2]) 1)
    (check-equal? (strict-cond [#f 1] [else 2]) 2)
    (check-equal? (strict-cond [#t 1]) 1)
    (check-exn exn:fail:contract? (λ () (strict-cond [#f 1])))
    (check-exn exn:fail:contract? (λ () (strict-cond)))))
