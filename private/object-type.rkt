#lang racket/base

(provide define-reference-type)

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         rebellion/collection/keyset/low-dependency
         rebellion/type/object/base
         rebellion/type/object/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class reference-id
    #:attributes (default-name
                  default-predicate-name
                  default-constructor-name
                  default-descriptor-name)
    (pattern id:id
      #:do [(define (derived-id fmt) (format-id #'id fmt #'id #:subs? #t))]
      #:with default-name #'id
      #:with default-predicate-name (derived-id "~a?")
      #:with default-constructor-name (derived-id "make-~a")
      #:with default-descriptor-name (derived-id "descriptor:~a"))))

(define-simple-macro
  (define-reference-type id:reference-id (field:id ...)
    (~alt
     (~optional (~seq #:constructor-name constructor:id)
                #:name "#:constructor-name option"
                #:defaults ([constructor #'id.default-constructor-name]))

     (~optional (~seq #:predicate-name predicate:id)
                #:name "#:predicate-name option"
                #:defaults ([predicate #'id.default-predicate-name]))

     (~optional (~seq #:descriptor-name descriptor:id)
                #:name "#:descriptor-name option"
                #:defaults ([descriptor #'id.default-descriptor-name]))

     (~optional (~seq #:inspector inspector:expr)
                #:name "#:inspector option"
                #:defaults ([inspector #'(current-inspector)]))

     (~optional (~seq #:property-maker prop-maker:expr)
                #:name "#:property-maker option"
                #:defaults
                ([prop-maker #'default-reference-properties])))
    ...)

  #:with (field* ...) (cons (syntax-local-introduce #'name)
                            (syntax->list #'(field ...)))
  #:with (field-kw ...)
  (for/list ([field-stx (in-syntax #'(field* ...))])
    (string->keyword (symbol->string (syntax-e field-stx))))
  #:with fields #'(keyset field-kw ...)
  #:with (field-accessor ...)
  (for/list ([field-stx (in-syntax #'(field* ...))])
    (format-id #'id "~a-~a" #'id.default-name field-stx #:subs? #t))
  (begin
    (define type (reference-type 'id.default-name fields))
    (define descriptor
      (make-reference-implementation type
                                     #:property-maker prop-maker
                                     #:inspector inspector))
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
    (check-pred initialized-reference-descriptor? descriptor:converter)
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
