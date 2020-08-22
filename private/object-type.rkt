#lang racket/base

(provide define-object-type)

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     rebellion/collection/keyset/low-dependency
                     (submod rebellion/private/object-type-binding
                             private-constructor)
                     rebellion/type/object/base)
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
  (define-syntax-class object-id
    #:attributes
    (default-descriptor-name
     default-predicate-name
     default-constructor-name
     default-accessor-name)
    (pattern id:id
      #:do [(define (derived-id fmt) (format-id #'id fmt #'id #:subs? #t))]
      #:with default-descriptor-name (derived-id "descriptor:~a")
      #:with default-predicate-name (derived-id "~a?")
      #:with default-constructor-name (derived-id "make-~a")
      #:with default-accessor-name (derived-id "~a-ref"))))

(define-simple-macro
  (define-object-type id:object-id (private-field:id ...)
    (~alt
     (~optional
      (~seq #:descriptor-name descriptor:id)
      #:name "#:descriptor-name option"
      #:defaults ([descriptor #'id.default-descriptor-name]))

     (~optional
      (~seq #:predicate-name predicate:id)
      #:name "#:predicate-name option"
      #:defaults ([predicate #'id.default-predicate-name]))

     (~optional
      (~seq #:constructor-name constructor:id)
      #:name "#:constructor-name option"
      #:defaults ([constructor #'id.default-constructor-name]))

     (~optional
      (~seq #:accessor-name accessor:id)
      #:name "#:accessor-name option"
      #:defaults ([accessor #'id.default-accessor-name]))

     (~optional
      (~seq #:inspector inspector:expr)
      #:name "#:inspector option"
      #:defaults ([inspector #'(current-inspector)]))

     (~optional
      (~seq #:property-maker prop-maker:expr)
      #:name "#:property-maker option"
      #:defaults ([prop-maker #'default-object-properties])))
    ...)

  #:with (field ...)
  (cons (syntax-local-introduce #'name) (syntax->list #'(private-field ...)))
  #:with (field-kw ...)
  (for/list ([field-stx (in-syntax #'(field ...))])
    (string->keyword (symbol->string (syntax-e field-stx))))
  #:with fields #'(keyset field-kw ...)
  #:with (field-accessor ...)
  (for/list
      ([field-stx (sort (syntax->list #'(field ...)) symbol<? #:key syntax-e)])
    (format-id #'id "~a-~a" #'id field-stx #:subs? #t))
  #:with (field-index ...)
  (for/list ([n (in-range (length (syntax->list #'(field ...))))]) #`'#,n)
  (begin
    (define descriptor
      (make-object-implementation
       (object-type
        'id fields #:predicate-name 'predicate #:constructor-name 'constructor)
       #:property-maker prop-maker
       #:inspector inspector))
    (define predicate (object-descriptor-predicate descriptor))
    (define constructor (object-descriptor-constructor descriptor))
    (define accessor (object-descriptor-accessor descriptor))
    (define field-accessor (make-object-field-accessor descriptor field-index))
    ...
    (define-syntax id
      (object-binding
       #:type
       (object-type
        'id fields #:predicate-name 'predicate #:constructor-name 'constructor)
       #:descriptor #'descriptor
       #:predicate #'predicate
       #:constructor #'constructor
       #:accessor #'accessor
       #:field-accessors (list #'field-accessor ...)))))

(module+ test
  (test-case "define-object-type"
    (define-object-type converter (forwards backwards))
    (define string<->symbol
      (make-converter #:forwards string->symbol
                      #:backwards symbol->string
                      #:name 'string<->symbol))
    (check-pred converter? string<->symbol)
    (check-pred initialized-object-descriptor? descriptor:converter)
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
