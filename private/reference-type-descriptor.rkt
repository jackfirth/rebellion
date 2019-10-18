#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
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
  [default-reference-properties
   (-> reference-descriptor? (listof (cons/c struct-type-property? any/c)))]
  [default-reference-equal+hash (-> reference-descriptor? equal+hash/c)]
  [default-reference-custom-write
   (-> reference-descriptor? custom-write-function/c)]
  [default-reference-object-name (-> reference-descriptor? natural?)]
  [make-reference-field-accessor
   (-> reference-descriptor? keyword? procedure?)]))

(require racket/list
         racket/math
         rebellion/collection/keyset/low-dependency
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/type/record
         rebellion/type/reference/base
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define (make-descriptor-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define type-field (keyset-index-of (record-type-fields type) '#:type))
  (define accessor (record-descriptor-accessor descriptor))
  (define (name-getter this) (reference-type-name (accessor this type-field)))
  (define custom-write
    (make-named-object-custom-write type-name #:name-getter name-getter))
  (list (cons prop:equal+hash (default-record-equal+hash descriptor))
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
         #:property-maker [prop-maker default-reference-properties]
         #:inspector [inspector (current-inspector)])
  (define (tuple-prop-maker descriptor)
    (prop-maker (tuple-descriptor->reference-descriptor descriptor type)))
  (tuple-descriptor->reference-descriptor
   (make-tuple-implementation (reference-type->tuple-type type)
                              #:property-maker tuple-prop-maker
                              #:inspector inspector)
   type))

(define (default-reference-properties descriptor)
  (list (cons prop:equal+hash (default-reference-equal+hash descriptor))
        (cons prop:custom-write
              (default-reference-custom-write descriptor))
        (cons prop:object-name
              (default-reference-object-name descriptor))))

(define (default-reference-equal+hash descriptor)
  (define accessor (reference-descriptor-accessor descriptor))
  (define size (reference-type-size (reference-descriptor-type descriptor)))
  (make-accessor-based-equal+hash accessor size))

(define (default-reference-custom-write descriptor)
  (define type-name
    (reference-type-name (reference-descriptor-type descriptor)))
  (make-named-object-custom-write type-name))

(define (default-reference-object-name descriptor)
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
