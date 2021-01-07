#lang racket/base

(provide define-object-type)

(require (for-syntax racket/base
                     racket/sequence
                     rebellion/collection/keyset/low-dependency
                     (submod rebellion/type/object/binding
                             private-constructor)
                     rebellion/type/private/naming
                     rebellion/type/object/base)
         rebellion/collection/keyset/low-dependency
         rebellion/type/object/base
         rebellion/type/object/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-object-type id:id (private-field:id ...)
    (~alt
     (~optional (~and #:omit-root-binding omit-root-binding-kw))
     
     (~optional
      (~seq #:descriptor-name descriptor:id)
      #:name "#:descriptor-name option"
      #:defaults ([descriptor (default-descriptor-identifier #'id)]))

     (~optional
      (~seq #:predicate-name predicate:id)
      #:name "#:predicate-name option"
      #:defaults ([predicate (default-predicate-identifier #'id)]))

     (~optional
      (~seq #:constructor-name constructor:id)
      #:name "#:constructor-name option"
      #:defaults ([constructor (default-opaque-constructor-identifier #'id)]))

     (~optional
      (~seq #:accessor-name accessor:id)
      #:name "#:accessor-name option"
      #:defaults ([accessor (default-accessor-identifier #'id)]))

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
  (sort (cons (datum->syntax #'id 'name)
              (syntax->list #'(private-field ...)))
        symbol<?
        #:key syntax-e)
  
  #:with (field-kw ...)
  (for/list ([field-stx (in-syntax #'(field ...))])
    (string->keyword (symbol->string (syntax-e field-stx))))
  
  #:with fields #'(keyset field-kw ...)

  #:with (field-accessor ...)
  (for/list ([field-stx (in-syntax #'(field ...))])
    (default-field-accessor-identifier #'id field-stx))
  
  #:with (field-index ...)
  (for/list ([n (in-range (length (syntax->list #'(field ...))))]) #`'#,n)
  
  #:with root-binding
  (if (attribute omit-root-binding-kw)
      #'(begin)
      #'(define-syntax id
          (object-binding
           #:type
           (object-type
            'id fields
            #:predicate-name 'predicate
            #:constructor-name 'constructor
            #:accessor-name 'accessor)
           #:descriptor #'descriptor
           #:predicate #'predicate
           #:constructor #'constructor
           #:accessor #'accessor
           #:fields (list #'field ...)
           #:field-accessors (list #'field-accessor ...))))
  
  (begin
    (define descriptor
      (make-object-implementation
       (object-type
        'id fields
        #:predicate-name 'predicate
        #:constructor-name 'constructor
        #:accessor-name 'accessor)
       #:inspector inspector
       #:property-maker prop-maker))
    (define predicate (object-descriptor-predicate descriptor))
    (define constructor (object-descriptor-constructor descriptor))
    (define accessor (object-descriptor-accessor descriptor))
    (define field-accessor (make-object-field-accessor descriptor field-index))
    ...
    root-binding))

(module+ test
  (test-case (name-string define-object-type)
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
