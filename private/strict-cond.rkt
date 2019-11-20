#lang racket/base

(provide strict-cond)

(require (for-syntax racket/base)
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define-simple-macro (strict-cond [condition:expr body ...+] ...+)
  #:with raise-error
  (syntax/loc this-syntax
    (raise-arguments-error 'strict-cond "none of the conditions were true"))
    
  (cond [condition body ...] ... [else raise-error]))
