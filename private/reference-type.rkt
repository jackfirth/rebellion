#lang racket/base

(provide define-reference-type)

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         rebellion/collection/keyset/low-dependency
         rebellion/type/reference/base
         rebellion/type/reference/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-reference-type id:id (field:id ...))
  #:with (field* ...) (cons (syntax-local-introduce #'name)
                            (syntax->list #'(field ...)))
  #:with (field-kw ...)
  (for/list ([field-stx (in-syntax #'(field* ...))])
    (string->keyword (symbol->string (syntax-e field-stx))))
  #:with fields #'(keyset field-kw ...)
  #:with constructor (format-id #'id "make-~a" #'id)
  #:with predicate (format-id #'id "~a?" #'id)
  #:with (field-accessor ...)
  (for/list ([field-stx (in-syntax #'(field* ...))])
    (format-id field-stx "~a-~a" #'id field-stx))
  (begin
    (define type (reference-type 'id fields))
    (define descriptor (make-reference-implementation type))
    (define constructor (reference-descriptor-constructor descriptor))
    (define predicate (reference-descriptor-predicate descriptor))
    (define field-accessor (make-reference-field-accessor descriptor 'field-kw))
    ...))

(module+ test
  (test-case "define-reference-type"
    (define-reference-type converter (forwards backwards))
    (define string<->symbol
      (make-converter #:forwards string->symbol
                      #:backwards symbol->string
                      #:name 'string<->symbol))
    (check-pred converter? string<->symbol)
    (check-equal? (converter-name string<->symbol) 'string<->symbol)
    (check-equal? (object-name string<->symbol) 'string<->symbol)
    (check-equal? (converter-forwards string<->symbol) string->symbol)
    (check-equal? (converter-backwards string<->symbol) symbol->string)
    (check-equal? (~a string<->symbol) "#<converter:string<->symbol>")
    (check-equal? (~v string<->symbol) "#<converter:string<->symbol>")
    (check-equal? (~s string<->symbol) "#<converter:string<->symbol>")
    (define anonymous-converter
      (make-converter #:forwards string->symbol
                      #:backwards symbol->string))
    (check-false (converter-name anonymous-converter))
    (check-false (object-name anonymous-converter))
    (check-equal? (~a anonymous-converter) "#<converter>")
    (check-equal? (~v anonymous-converter) "#<converter>")
    (check-equal? (~s anonymous-converter) "#<converter>")))
