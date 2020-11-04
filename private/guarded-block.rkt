#lang racket/base

;; Guarded blocks are like regular blocks, except they support early exits.
;; Early exits are  specified with the guard statement. Guarded blocks are
;; transformed into equivalent nested cond expressions.

(provide define/guard
         guard
         guarded-block
         then)

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header)
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define-syntax (guard stx)
  (raise-syntax-error
   #false "must be used immediately within a guarded block" stx))

(define-syntax (then stx)
  (raise-syntax-error
   #false "must be used immediately within a guard statement" stx))

(define-simple-macro (guarded-block form:expr ...)
  (let () (guarded-begin form ...)))

(define-syntax guarded-begin
  (syntax-parser
    #:track-literals
    [(_) #'(begin)]
    [(_ initial-form leftover-form ...)
     (define expanded-initial-form
       (local-expand
        #'initial-form (syntax-local-context) (list #'guard #'define-values)))
     (syntax-protect
      (syntax-parse (syntax-disarm expanded-initial-form #false)
        #:literal-sets (kernel-literals)
        #:literals (guard else then)
        #:track-literals
        [(begin ~! subform:expr ...)
         #'(guarded-begin subform ... leftover-form ...)]
        [(define-values ~! . _)
         #`(begin #,expanded-initial-form (guarded-begin leftover-form ...))]
        [(define-syntaxes ~! . _)
         #`(begin #,expanded-initial-form (guarded-begin leftover-form ...))]
        [(guard condition:expr then ~! then-form:expr ...+)
         #'(cond
             [condition (guarded-begin then-form ...)]
             [else (guarded-begin leftover-form ...)])]
        [(guard condition:expr else ~! else-form:expr ...+)
         #'(cond
             [condition (guarded-begin leftover-form ...)]
             [else (guarded-begin else-form ...)])]
        [e:expr #'(begin e (guarded-begin leftover-form ...))]))]))

(define-simple-macro (define/guard header:function-header body:expr ...+)
  (define header (guarded-begin body ...)))
