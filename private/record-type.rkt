#lang racket/base

(provide define-record-type)

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         rebellion/collection/keyset/low-dependency
         rebellion/type/record/base
         rebellion/type/record/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-record-type id:id (field-id:id ...)
    (~alt (~optional (~seq #:predicate-name predicate-name:id)
                     #:defaults ([predicate-name (format-id #'id "~a?" #'id)])
                     #:name "#:predicate-name option")
          (~optional (~seq #:constructor-name constructor-name:id)
                     #:defaults ([constructor-name #'id])
                     #:name "#:constructor-name option")
          (~optional (~seq #:property-maker prop-maker:expr)
                     #:defaults ([prop-maker #'make-default-record-properties])
                     #:name "#:property-maker option"))
    ...)
  #:with fields
  #`(keyset
     #,@(for/list ([field-id-stx (in-syntax #'(field-id ...))])
          (string->keyword (symbol->string (syntax-e field-id-stx)))))
  #:with (sorted-field-id ...)
  (sort (syntax->list #'(field-id ...)) symbol<? #:key syntax-e)
  #:with (field-accessor-id ...)
  (for/list ([field-id-stx (in-syntax #'(sorted-field-id ...))])
    (format-id field-id-stx "~a-~a" #'id field-id-stx))
  #:with (position ...)
  (for/list ([n (in-range (length (syntax->list #'(field-id ...))))])
    #`(quote #,n))
  (begin
    (define type
      (record-type 'id fields
                   #:predicate-name 'predicate-name
                   #:constructor-name 'constructor-name))
    (define descriptor
      (make-record-implementation type #:property-maker prop-maker))
    (define predicate-name (record-descriptor-predicate descriptor))
    (define constructor-name (record-descriptor-constructor descriptor))
    (define field-accessor-id (make-record-field-accessor descriptor position))
    ...))

(module+ test
  (define-record-type person (name age favorite-color))
  (define ted (person #:name "Ted" #:age 42 #:favorite-color 'grey))
  (check-equal? ted (person #:age 42 #:name "Ted" #:favorite-color 'grey))
  (check-equal? (person-name ted) "Ted")
  (check-equal? (person-age ted) 42)
  (check-equal? (person-favorite-color ted) 'grey)
  (check-true (person? ted))
  (define-record-type plant (name))
  (check-false (person? (plant #:name "Cactus")))
  (check-equal? (~a ted)
                "#<person: #:age 42 #:favorite-color grey #:name Ted>")
  (check-equal? (~v ted)
                "(person #:age 42 #:favorite-color 'grey #:name \"Ted\")")
  (check-equal? (~s ted)
                "#<person: #:age 42 #:favorite-color grey #:name \"Ted\">"))
