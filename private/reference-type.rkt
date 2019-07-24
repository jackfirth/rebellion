#lang racket/base

(require racket/contract/base)

(provide
 define-reference-type
 (contract-out
  [reference-type
   (->* (interned-symbol? keyset?)
        (#:object-name-field natural?
         #:constructor-name (or/c interned-symbol? #f)
         #:accessor-name (or/c interned-symbol? #f)
         #:predicate-name (or/c interned-symbol? #f))
        reference-type?)]
  [reference-type? predicate/c]
  [reference-type-name (-> reference-type? interned-symbol?)]
  [reference-type-fields (-> reference-type? keyset?)]
  [reference-type-object-name-field (-> reference-type? natural?)]
  [reference-type-constructor-name (-> reference-type? interned-symbol?)]
  [reference-type-predicate-name (-> reference-type? interned-symbol?)]
  [reference-type-accessor-name (-> reference-type? interned-symbol?)]
  [reference-type-size (-> reference-type? natural?)]
  [reference-descriptor? predicate/c]
  [reference-descriptor-type (-> reference-descriptor? reference-type?)]
  [reference-descriptor-constructor (-> reference-descriptor? procedure?)]
  [reference-descriptor-predicate (-> reference-descriptor? predicate/c)]
  [reference-descriptor-accessor
   (-> reference-descriptor? (-> any/c natural? any/c))]
  [initialized-reference-descriptor? predicate/c]
  [uninitialized-reference-descriptor? predicate/c]
  [make-reference-implementation
   (->* (reference-type?)
        (#:property-maker (-> uninitialized-reference-descriptor?
                              (listof (cons/c struct-type-property? any/c)))
         #:inspector inspector?)
        initialized-reference-descriptor?)]
  [make-default-reference-properties
   (-> reference-descriptor? (listof (cons/c struct-type-property? any/c)))]
  [make-default-reference-equal+hash (-> reference-descriptor? equal+hash/c)]
  [make-default-reference-custom-write
   (-> reference-descriptor? custom-write-function/c)]
  [make-default-reference-object-name (-> reference-descriptor? natural?)]))

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         racket/list
         racket/math
         rebellion/base/symbol
         rebellion/collection/keyset/low-dependency
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/type/record
         rebellion/type/tuple
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define-record-type reference-type
  (name fields object-name-field constructor-name predicate-name accessor-name)
  #:constructor-name constructor:reference-type)

(define (reference-type name fields
                        #:object-name-field [name-field
                                             (keyset-index-of fields '#:name)]
                        #:constructor-name [constructor-name #f]
                        #:accessor-name [accessor-name #f]
                        #:predicate-name [predicate-name #f])
  (constructor:reference-type
   #:name name
   #:fields fields
   #:object-name-field name-field
   #:constructor-name (or constructor-name (default-constructor-name name))
   #:accessor-name (or accessor-name (default-accessor-name name))
   #:predicate-name (or predicate-name (default-predicate-name name))))

(define (default-constructor-name type-name)
  (string->symbol (format "make-~a" type-name)))

(define (default-accessor-name type-name)
  (string->symbol (format "~a-ref" type-name)))

(define (default-predicate-name type-name)
  (string->symbol (format "~a?" type-name)))

(define (reference-type-size type) (keyset-size (reference-type-fields type)))

(define (make-descriptor-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define type-field (keyset-index-of (record-type-fields type) '#:type))
  (define accessor (record-descriptor-accessor descriptor))
  (define (name-getter this) (reference-type-name (accessor this type-field)))
  (define custom-write
    (make-named-object-custom-write type-name #:name-getter name-getter))
  (list (cons prop:equal+hash (make-record-equal+hash descriptor))
        (cons prop:custom-write custom-write)
        (cons prop:object-name name-getter)))

(define-record-type initialized-reference-descriptor
  (type constructor accessor predicate)
  #:constructor-name make-initialized-reference-descriptor
  #:property-maker make-descriptor-properties)

(define-record-type uninitialized-reference-descriptor
  (type constructor accessor predicate)
  #:constructor-name make-uninitialized-reference-descriptor
  #:property-maker make-descriptor-properties)

(define (reference-descriptor? v)
  (or (initialized-reference-descriptor? v)
      (uninitialized-reference-descriptor? v)))

(define (reference-descriptor-type descriptor)
  (if (initialized-reference-descriptor? descriptor)
      (initialized-reference-descriptor-type descriptor)
      (uninitialized-reference-descriptor-type descriptor)))

(define (reference-descriptor-constructor descriptor)
  (if (initialized-reference-descriptor? descriptor)
      (initialized-reference-descriptor-constructor descriptor)
      (uninitialized-reference-descriptor-constructor descriptor)))

(define (reference-descriptor-predicate descriptor)
  (if (initialized-reference-descriptor? descriptor)
      (initialized-reference-descriptor-predicate descriptor)
      (uninitialized-reference-descriptor-predicate descriptor)))

(define (reference-descriptor-accessor descriptor)
  (if (initialized-reference-descriptor? descriptor)
      (initialized-reference-descriptor-accessor descriptor)
      (uninitialized-reference-descriptor-accessor descriptor)))

;@------------------------------------------------------------------------------

(define (tuple-constructor->reference-constructor constructor type)
  (define fields (reference-type-fields type))
  (define size (keyset-size fields))
  (define name-field-position (reference-type-object-name-field type))
  (define name-field (keyset-ref fields name-field-position))
  (define required-fields (keyset-remove fields name-field))
  (define (positional-keyword-constructor kws vs)
    (apply constructor
           (if (equal? (length kws) size)
               vs
               (let-values ([(vs-before-name vs-after-name)
                             (split-at vs name-field-position)])
                 (append vs-before-name (list #f) vs-after-name)))))
  (define arity-unchecked-constructor
    (make-keyword-procedure positional-keyword-constructor))
  (define unnamed-constructor
    (procedure-reduce-keyword-arity arity-unchecked-constructor
                                    0
                                    (keyset->list required-fields)
                                    (keyset->list fields)))
  (procedure-rename unnamed-constructor
                    (reference-type-constructor-name type)))

(define (tuple-descriptor->reference-descriptor descriptor type)
  (define make-descriptor
    (if (initialized-tuple-descriptor? descriptor)
        make-initialized-reference-descriptor
        make-uninitialized-reference-descriptor))
  (define constructor
    (tuple-constructor->reference-constructor
     (tuple-descriptor-constructor descriptor) type))
  (make-descriptor #:type type
                   #:constructor constructor
                   #:predicate (tuple-descriptor-predicate descriptor)
                   #:accessor (tuple-descriptor-accessor descriptor)))

(define (reference-type->tuple-type type)
  (tuple-type (reference-type-name type)
              (reference-type-size type)
              #:predicate-name (reference-type-predicate-name type)
              #:constructor-name (reference-type-constructor-name type)
              #:accessor-name (reference-type-accessor-name type)))

(define (make-reference-implementation
         type
         #:property-maker [prop-maker make-default-reference-properties]
         #:inspector [inspector (current-inspector)])
  (define (tuple-prop-maker descriptor)
    (prop-maker (tuple-descriptor->reference-descriptor descriptor type)))
  (tuple-descriptor->reference-descriptor
   (make-tuple-implementation (reference-type->tuple-type type)
                              #:property-maker tuple-prop-maker
                              #:inspector inspector)
   type))

(define (make-default-reference-properties descriptor)
  (list (cons prop:equal+hash (make-default-reference-equal+hash descriptor))
        (cons prop:custom-write
              (make-default-reference-custom-write descriptor))
        (cons prop:object-name
              (make-default-reference-object-name descriptor))))

(define (make-default-reference-equal+hash descriptor)
  (define accessor (reference-descriptor-accessor descriptor))
  (define size (reference-type-size (reference-descriptor-type descriptor)))
  (make-accessor-based-equal+hash accessor size))

(define (make-default-reference-custom-write descriptor)
  (define type-name
    (reference-type-name (reference-descriptor-type descriptor)))
  (make-named-object-custom-write type-name))

(define (make-default-reference-object-name descriptor)
  (reference-type-object-name-field (reference-descriptor-type descriptor)))

(define (make-reference-field-accessor descriptor field)
  (define type (reference-descriptor-type descriptor))
  (define accessor (reference-descriptor-accessor descriptor))
  (define fields (reference-type-fields type))
  (define position (keyset-index-of fields field))
  (define name
    (string->symbol
     (format "~a-~a" (reference-type-name type) (keyword->string field))))
  (procedure-rename (λ (this) (accessor this position)) name))

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
