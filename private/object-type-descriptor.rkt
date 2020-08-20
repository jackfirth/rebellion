#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [object-descriptor? predicate/c]
  [object-descriptor-type (-> object-descriptor? object-type?)]
  [object-descriptor-constructor (-> object-descriptor? procedure?)]
  [object-descriptor-predicate (-> object-descriptor? predicate/c)]
  [object-descriptor-accessor
   (-> object-descriptor? (-> any/c natural? any/c))]
  [initialized-object-descriptor? predicate/c]
  [uninitialized-object-descriptor? predicate/c]
  [make-object-implementation
   (->* (object-type?)
        (#:property-maker (-> uninitialized-object-descriptor?
                              (listof (cons/c struct-type-property? any/c)))
         #:inspector inspector?)
        initialized-object-descriptor?)]
  [default-object-properties
   (-> object-descriptor? (listof (cons/c struct-type-property? any/c)))]
  [default-object-equal+hash (-> object-descriptor? equal+hash/c)]
  [default-object-custom-write
   (-> object-descriptor? custom-write-function/c)]
  [default-object-name-property (-> object-descriptor? natural?)]
  [make-object-field-accessor
   (-> object-descriptor? keyword? procedure?)]
  [object-impersonate
   (->i #:chaperone
        ([instance (descriptor) (object-descriptor-predicate descriptor)]
         [descriptor initialized-object-descriptor?])
        (#:properties [properties impersonator-property-hash/c]
         #:chaperone? [chaperone? boolean?])
        [_ (descriptor) (object-descriptor-predicate descriptor)])]))

(require racket/list
         racket/math
         rebellion/collection/keyset/low-dependency
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/private/impersonation
         rebellion/type/record
         rebellion/type/object/base
         rebellion/type/tuple/base
         rebellion/type/tuple/descriptor)

;@------------------------------------------------------------------------------

(define (make-object-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define type-field (keyset-index-of (record-type-fields type) '#:type))
  (define accessor (record-descriptor-accessor descriptor))
  (define (name-getter this) (object-type-name (accessor this type-field)))
  (define custom-write
    (make-named-object-custom-write type-name #:name-getter name-getter))
  (list (cons prop:equal+hash (default-record-equal+hash descriptor))
        (cons prop:custom-write custom-write)
        (cons prop:object-name name-getter)))

(define-record-type initialized-object-descriptor
  (type backing-tuple-descriptor constructor accessor predicate)
  #:constructor-name make-initialized-object-descriptor
  #:property-maker make-object-properties)

(define-record-type uninitialized-object-descriptor
  (type constructor accessor predicate)
  #:constructor-name make-uninitialized-object-descriptor
  #:property-maker make-object-properties)

(define (object-descriptor? v)
  (or (initialized-object-descriptor? v)
      (uninitialized-object-descriptor? v)))

(define (object-descriptor-type descriptor)
  (if (initialized-object-descriptor? descriptor)
      (initialized-object-descriptor-type descriptor)
      (uninitialized-object-descriptor-type descriptor)))

(define (object-descriptor-constructor descriptor)
  (if (initialized-object-descriptor? descriptor)
      (initialized-object-descriptor-constructor descriptor)
      (uninitialized-object-descriptor-constructor descriptor)))

(define (object-descriptor-predicate descriptor)
  (if (initialized-object-descriptor? descriptor)
      (initialized-object-descriptor-predicate descriptor)
      (uninitialized-object-descriptor-predicate descriptor)))

(define (object-descriptor-accessor descriptor)
  (if (initialized-object-descriptor? descriptor)
      (initialized-object-descriptor-accessor descriptor)
      (uninitialized-object-descriptor-accessor descriptor)))

;@------------------------------------------------------------------------------

(define (tuple-constructor->object-constructor constructor type)
  (define fields (object-type-fields type))
  (define size (keyset-size fields))
  (define name-field-position (object-type-object-name-field type))
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
                    (object-type-constructor-name type)))

(define (tuple-descriptor->object-descriptor descriptor type)
  (define constructor
    (tuple-constructor->object-constructor
     (tuple-descriptor-constructor descriptor) type))
  (if (initialized-tuple-descriptor? descriptor)
      (make-initialized-object-descriptor
       #:type type
       #:constructor constructor
       #:predicate (tuple-descriptor-predicate descriptor)
       #:accessor (tuple-descriptor-accessor descriptor)
       #:backing-tuple-descriptor descriptor)
      (make-uninitialized-object-descriptor
       #:type type
       #:constructor constructor
       #:predicate (tuple-descriptor-predicate descriptor)
       #:accessor (tuple-descriptor-accessor descriptor))))

(define (object-type->tuple-type type)
  (tuple-type (object-type-name type)
              (for/list ([field (object-type-fields type)])
                (string->symbol (keyword->string field)))
              #:predicate-name (object-type-predicate-name type)
              #:constructor-name (object-type-constructor-name type)
              #:accessor-name (object-type-accessor-name type)))

(define (make-object-implementation
         type
         #:property-maker [prop-maker default-object-properties]
         #:inspector [inspector (current-inspector)])
  (define (tuple-prop-maker descriptor)
    (prop-maker (tuple-descriptor->object-descriptor descriptor type)))
  (tuple-descriptor->object-descriptor
   (make-tuple-implementation (object-type->tuple-type type)
                              #:property-maker tuple-prop-maker
                              #:inspector inspector)
   type))

(define (default-object-properties descriptor)
  (list (cons prop:equal+hash (default-object-equal+hash descriptor))
        (cons prop:custom-write
              (default-object-custom-write descriptor))
        (cons prop:object-name
              (default-object-name-property descriptor))))

(define (default-object-equal+hash descriptor)
  (define accessor (object-descriptor-accessor descriptor))
  (define size (object-type-size (object-descriptor-type descriptor)))
  (make-accessor-based-equal+hash accessor size))

(define (default-object-custom-write descriptor)
  (define type-name
    (object-type-name (object-descriptor-type descriptor)))
  (make-named-object-custom-write type-name))

(define (default-object-name-property descriptor)
  (object-type-object-name-field (object-descriptor-type descriptor)))

(define (make-object-field-accessor descriptor field)
  (define type (object-descriptor-type descriptor))
  (define accessor (object-descriptor-accessor descriptor))
  (define fields (object-type-fields type))
  (define position (keyset-index-of fields field))
  (define name
    (string->symbol
     (format "~a-~a" (object-type-name type) (keyword->string field))))
  (procedure-rename (λ (this) (accessor this position)) name))

(define (object-impersonate instance descriptor
                            #:properties [props (hash)]
                            #:chaperone? [chaperone? #t])
  (define tuple-descriptor
    (initialized-object-descriptor-backing-tuple-descriptor descriptor))
  (tuple-impersonate instance tuple-descriptor
                     #:properties props
                     #:chaperone? chaperone?))
