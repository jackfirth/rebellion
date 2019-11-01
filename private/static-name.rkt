#lang racket/base

;; This module provides utilities for working with static names. A static name
;; is a symbol or string that is derived from a bound identifier. Static names
;; are primarily used in error messages in order to identify pieces of code
;; relevant to the error. Static names are always derived at compile-time.
;;
;; Using static names is similar to using the quote macro, but whereas quoting
;; allows constructing symbols from arbitrary identifiers, static names can only
;; be constructed from *bound* identifiers. This ensures that typos in names
;; used within error messages cause compile-time errors instead of silently
;; incorrect message text. Furthermore, constructing a static name counts as a
;; *disappeared use* of the identifier, meaning that DrRacket will draw binding
;; arrows to it and DrRacket's identifier-rename operation will work correctly.
;;
;; This module is private to Rebellion for the time being. After using it
;; internally and getting a better sense of how the API should look, we may
;; consider exposing it publicly (with actual Scribble docs).

(provide name ;; Like quote, but the given identifier must be bound
         
         name-string ;; Like name, but produces a string instead of a symbol

         ;; A variable-like macro that is set by define/name.
         enclosing-function-name

         ;; Like define, but sets enclosing-function-name when used for function
         ;; definitions. Variable definitions such as (define/name foo 42) are
         ;; allowed, but they don't change enclosing-function-name.
         ;;
         ;; Example:
         ;;
         ;; (define/name (check-even n)
         ;;   (unless (even? n)
         ;;     (raise-argument-error enclosing-function-name
         ;;                           (name-string even?)
         ;;                           n)))
         define/name)

(require (for-syntax racket/base
                     syntax/parse/lib/function-header)
         racket/splicing
         racket/stxparam
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(begin-for-syntax
  ;; The function-header syntax class really should provide a .name attribute
  ;; that recursively traverses into subheaders, but as of Racket 7.4 it
  ;; doesn't. So we have to make our own version of function-header.
  (define-syntax-class function-header-with-recursive-name
    #:attributes (function-name)
    (pattern (function-name:id . args:formals))
    (pattern (header:function-header-with-recursive-name . args:formals)
      #:with function-name #'header.function-name)))

(define-syntax (raise-enclosing-function-name-unbound-error stx)
  (raise-syntax-error #f "not bound by any enclosing definition forms" stx))

(define-rename-transformer-parameter enclosing-function-name
  (make-rename-transformer #'raise-enclosing-function-name-unbound-error))

(begin-for-syntax
  (define (check-name-has-binding! id-stx context-stx)
    (unless (identifier-binding id-stx)
      (raise-syntax-error #f "named identifier not bound" context-stx id-stx))
    (syntax-parse-state-cons! 'literals id-stx)))

(define-simple-macro (name/derived id:id #:context context)
  #:do [(check-name-has-binding! #'id #'context)]
  (quote id))

(define-simple-macro (name id:id)
  #:with context this-syntax
  (#%expression (name/derived id #:context context)))

(define-simple-macro (name-string/derived id:id #:context context)
  #:do [(check-name-has-binding! #'id #'context)]
  #:with literal-string (string->immutable-string
                         (symbol->string (syntax-e #'id)))
  (quote literal-string))

(define-simple-macro (name-string id:id)
  #:with context this-syntax
  (#%expression (name-string/derived id #:context context)))

(define-simple-macro
  (define/name
    (~or id:id header:function-header-with-recursive-name)
    body ...)
  (~? (define id body ...)
      (splicing-let ([function-name (quote header.function-name)])
        (splicing-syntax-parameterize
            ([enclosing-function-name
              (make-rename-transformer #'function-name)])
          (define header body ...)))))

(module+ test
  (test-case (name-string define/name)
    (define/name (check-even n)
      (unless (even? n)
        (raise-argument-error enclosing-function-name (name-string even?) n)))
    (check-exn #rx"check-even:" (λ () (check-even 3)))
    (check-exn #rx"even\\?" (λ () (check-even 3)))))
