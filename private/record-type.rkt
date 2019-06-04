#lang racket/base

(require racket/contract/base)

(provide
 define-record-type
 (contract-out
  [record-type (->* (symbol? keyset?)
                    (#:predicate-name (or/c symbol? #f)
                     #:constructor-name (or/c symbol? #f)
                     #:accessor-name (or/c symbol? #f))
                    record-type?)]
  [record-type? predicate/c]
  [record-type-name (-> record-type? symbol?)]
  [record-type-fields (-> record-type? keyset?)]
  [record-type-predicate-name (-> record-type? (or/c symbol? #f))]
  [record-type-constructor-name (-> record-type? (or/c symbol? #f))]
  [record-type-accessor-name (-> record-type? (or/c symbol? #f))]
  [make-record-implementation
   (->* (record-type?)
        (#:inspector inspector?
         #:property-maker (-> uninitialized-record-descriptor? properties/c))
        initialized-record-descriptor?)]
  [record-descriptor? predicate/c]
  [initialized-record-descriptor? predicate/c]
  [uninitialized-record-descriptor? predicate/c]
  [record-descriptor-type (-> record-descriptor? record-type?)]
  [record-descriptor-predicate (-> record-descriptor? predicate/c)]
  [record-descriptor-constructor (-> record-descriptor? procedure?)]
  [record-descriptor-accessor (-> record-descriptor? procedure?)]
  [make-default-record-properties (-> record-descriptor? properties/c)]
  [make-record-equal+hash (-> record-descriptor? equal+hash/c)]
  [make-record-custom-write (-> record-descriptor? custom-write-function/c)]))

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         racket/struct
         rebellion/collection/keyset
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/type/tuple
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define properties/c (listof (cons/c struct-type-property? any/c)))

(define-tuple-type record-type
  (name fields predicate-name constructor-name accessor-name)
  #:constructor constructor:record-type)

(define (record-type name fields
                     #:predicate-name [predicate-name #f]
                     #:constructor-name [constructor-name #f]
                     #:accessor-name [accessor-name #f])
  (constructor:record-type name
                           fields
                           predicate-name
                           constructor-name
                           accessor-name))

(define-tuple-type initialized-record-descriptor
  (type predicate constructor accessor))

(define-tuple-type uninitialized-record-descriptor
  (type predicate constructor accessor))

(define (record-descriptor? v)
  (or (initialized-record-descriptor? v) (uninitialized-record-descriptor? v)))

(define ((make-getter initialized uninitialized) descriptor)
  (if (initialized-record-descriptor? descriptor)
      (initialized descriptor)
      (uninitialized descriptor)))

(define record-descriptor-type
  (make-getter initialized-record-descriptor-type
               uninitialized-record-descriptor-type))

(define record-descriptor-predicate
  (make-getter initialized-record-descriptor-predicate
               uninitialized-record-descriptor-predicate))

(define record-descriptor-constructor
  (make-getter initialized-record-descriptor-constructor
               uninitialized-record-descriptor-constructor))

(define record-descriptor-accessor
  (make-getter initialized-record-descriptor-accessor
               uninitialized-record-descriptor-accessor))

(define (make-record-implementation
         type
         #:inspector [inspector (current-inspector)]
         #:property-maker [prop-maker make-default-record-properties])
  (define (tuple-prop-maker descriptor)
    (prop-maker (tuple-descriptor->record-descriptor descriptor type)))
  (define descriptor
    (tuple-type-make-implementation (record-type->tuple-type type)
                                    #:inspector inspector
                                    #:property-maker tuple-prop-maker))
  (tuple-descriptor->record-descriptor descriptor type))

(define (record-type->tuple-type type)
  (tuple-type (record-type-name type)
              (keyset-size (record-type-fields type))
              #:predicate-name (record-type-predicate-name type)
              #:constructor-name (record-type-constructor-name type)
              #:accessor-name (record-type-accessor-name type)))

(define (tuple-descriptor->record-descriptor descriptor type)
  (define type-name (record-type-name type))
  (define maker
    (if (initialized-tuple-descriptor? descriptor)
        initialized-record-descriptor
        uninitialized-record-descriptor))
  (define predicate (tuple-descriptor-predicate descriptor))
  (define tuple-constructor (tuple-descriptor-constructor descriptor))
  (define fields (keyset->list (record-type-fields type)))
  (define raw-constructor
    (make-keyword-procedure (λ (unused-kws vs) (apply tuple-constructor vs))))
  (define constructor
    (procedure-rename
     (procedure-reduce-keyword-arity raw-constructor 0 fields fields)
     (object-name tuple-constructor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (maker type predicate constructor accessor))

(define (make-default-record-properties descriptor)
  (define equal+hash (make-record-equal+hash descriptor))
  (define custom-write (make-record-custom-write descriptor))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define (make-record-equal+hash descriptor)
  (define accessor (record-descriptor-accessor descriptor))
  (define size
    (keyset-size (record-type-fields (record-descriptor-type descriptor))))
  (make-accessor-based-equal+hash accessor size))

(define (unquoted-printing-keyword kw)
  (unquoted-printing-string (string-append "#:" (keyword->string kw))))

(define (make-record-custom-write descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define accessor (record-descriptor-accessor descriptor))
  (define fields (record-type-fields type))
  (define size (keyset-size fields))
  (make-constructor-style-printer
   (λ (_) type-name)
   (λ (this)
     (for*/list ([i (in-range size)]
                 [kw (in-value (keyset-ref fields i))]
                 [v (in-value (accessor this i))]
                 [contents (in-list (list kw v))])
       contents))))

(define (make-record-field-accessor descriptor field)
  (define accessor (record-descriptor-accessor descriptor))
  (define type (record-descriptor-type descriptor))
  (define fields (record-type-fields type))
  (define name
    (string->symbol
     (string-append (symbol->string (record-type-name type))
                    "-"
                    (keyword->string (keyset-ref fields field)))))
  (procedure-rename (λ (this) (accessor this field)) name))

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
  (check-false (person? (plant #:name "Cactus"))))
