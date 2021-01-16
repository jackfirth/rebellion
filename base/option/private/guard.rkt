#lang racket/base

(provide guard-present)

(require (for-syntax racket/base
                     racket/syntax)
         racket/block
         rebellion/base/option
         rebellion/private/guarded-block
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@----------------------------------------------------------------------------------------------------

(begin-for-syntax
  ;; This is used to give the generated identifiers readable names, and to make it clear which
  ;; option identifiers are associated with which present value identifiers. We could just use an
  ;; identifier like "tmp" for all of them (because macro hygiene will ensure the right one gets used)
  ;; but deriving the option name from the identifier given to guard-present makes it easier for users
  ;; to understand the generated code in the macro stepper.
  (define (make-option-id present-id)
    (format-id #'here "~a-option" present-id)))

(define-syntax-parser guard-present
  #:track-literals
  #:literals (then else)

  [(_ id:id expr then ~! body:expr ...+)
   #:declare expr (expr/c #'option?)
   #:with id-option (make-option-id #'id)
   #'(begin
       (define id-option expr.c)
       (guard (present? id-option) then
         (define id (present-value id-option))
         (block body ...)))]

  [(_ id:id expr else ~! body:expr ...+)
   #:declare expr (expr/c #'option?)
   #:with id-option (make-option-id #'id)
   #'(begin
       (define id-option expr.c)
       (guard (present? id-option) else body ...)
       (define id (present-value id-option)))]

  [(_ id:id expr)
   #:declare expr (expr/c #'option?)
   #:with id-option (make-option-id #'id)
   #'(begin
       (define id-option expr.c)
       (guard (present? id-option) else
         (raise-arguments-error 'guard-present "expected a present option"))
       (define id (present-value id-option)))])

(module+ test
  (test-case "guard-present"

    (test-case "then case"
      (define/guard (run opt)
        (guard-present v opt then v)
        #false)
      (check-equal? (run (present 4)) 4)
      (check-false (run absent)))

    (test-case "else case"
      (define/guard (run opt)
        (guard-present v opt else #false)
        v)
      (check-equal? (run (present 4)) 4)
      (check-false (run absent)))

    (test-case "else raise case"
      (define/guard (run opt)
        (guard-present v opt)
        v)
      (check-equal? (run (present 4)) 4)
      (check-exn exn:fail:contract? (λ () (run absent)))
      (check-exn #rx"guard-present:" (λ () (run absent)))
      (check-exn #rx"expected a present option" (λ () (run absent))))

    (test-case "hygiene"
      (define foo-option "other binding")
      (check-equal?
       (guarded-block
         (guard-present foo (present 4))
         foo-option)
       "other binding")
      (check-equal?
       (guarded-block
         (guard-present foo absent then foo)
         foo-option)
       "other binding")
      (check-equal?
       (guarded-block
         (guard-present foo absent else foo-option)
         foo)
       "other binding"))))
