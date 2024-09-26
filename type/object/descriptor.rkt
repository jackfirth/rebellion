#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [object-descriptor? (-> any/c boolean?)]
  [object-descriptor-type (-> object-descriptor? object-type?)]
  [object-descriptor-constructor (-> object-descriptor? procedure?)]
  [object-descriptor-predicate (-> object-descriptor? (-> any/c boolean?))]
  [object-descriptor-accessor
   (-> object-descriptor? (-> any/c natural? any/c))]
  [initialized-object-descriptor? (-> any/c boolean?)]
  [uninitialized-object-descriptor? (-> any/c boolean?)]
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
  [make-object-field-accessor (-> object-descriptor? natural? procedure?)]
  [object-impersonate
   (->i #:chaperone
        ([instance (descriptor) (object-descriptor-predicate descriptor)]
         [descriptor initialized-object-descriptor?])
        (#:properties [properties impersonator-property-hash/c]
         #:chaperone? [chaperone? boolean?])
        [_ (descriptor) (object-descriptor-predicate descriptor)])]))

(require guard
         racket/list
         racket/math
         rebellion/collection/keyset/low-dependency
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/private/impersonation
         rebellion/type/object/base
         rebellion/type/record
         rebellion/type/tuple/base
         rebellion/type/tuple/descriptor)

;@------------------------------------------------------------------------------

(define (write-descriptor this out _)
  (define name (object-name this))
  (write-string "#<object-descriptor:" out)
  (write-string (symbol->string name) out)
  (write-string ">" out)
  (void))

(struct initialized-object-descriptor
  (type predicate constructor accessor backing-tuple-descriptor)
  #:omit-define-syntaxes
  #:constructor-name constructor:initialized-object-descriptor

  #:property prop:object-name
  (λ (this) (object-type-name (initialized-object-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(struct uninitialized-object-descriptor
  (type predicate constructor accessor)
  #:omit-define-syntaxes
  #:constructor-name constructor:uninitialized-object-descriptor

  #:property prop:object-name
  (λ (this) (object-type-name (uninitialized-object-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(define (initialized-object-descriptor
         #:type type
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor
         #:backing-tuple-descriptor tuple-descriptor)
  (constructor:initialized-object-descriptor
   type predicate constructor accessor tuple-descriptor))

(define (uninitialized-object-descriptor
         #:type type
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor)
  (constructor:uninitialized-object-descriptor
   type predicate constructor accessor))

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
  (define name-position (object-type-name-field-position type))
  (define private-fields (object-type-private-fields type))
  (define (positional-keyword-constructor kws vs)
    (define args
      (guarded-block
       (guard (not (equal? (length kws) size)) #:else vs)
       (define-values (before-name after-name) (split-at vs name-position))
       (append before-name (list #false) after-name)))
    (apply constructor args))
  (define arity-unchecked-constructor
    (make-keyword-procedure positional-keyword-constructor))
  (define unnamed-constructor
    (procedure-reduce-keyword-arity arity-unchecked-constructor
                                    0
                                    (keyset->list private-fields)
                                    (keyset->list fields)))
  (procedure-rename unnamed-constructor
                    (object-type-constructor-name type)))

(define (tuple-descriptor->object-descriptor descriptor type)
  (define constructor
    (tuple-constructor->object-constructor
     (tuple-descriptor-constructor descriptor) type))
  (if (initialized-tuple-descriptor? descriptor)
      (initialized-object-descriptor
       #:type type
       #:constructor constructor
       #:predicate (tuple-descriptor-predicate descriptor)
       #:accessor (tuple-descriptor-accessor descriptor)
       #:backing-tuple-descriptor descriptor)
      (uninitialized-object-descriptor
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
  (list
   (cons prop:equal+hash (default-object-equal+hash descriptor))
   (cons prop:custom-write (default-object-custom-write descriptor))
   (cons prop:object-name (default-object-name-property descriptor))
   (cons prop:custom-print-quotable 'never)))

(define (default-object-equal+hash descriptor)
  (define accessor (object-descriptor-accessor descriptor))
  (define size (object-type-size (object-descriptor-type descriptor)))
  (make-accessor-based-equal+hash accessor size))

(define (default-object-custom-write descriptor)
  (define type-name (object-type-name (object-descriptor-type descriptor)))
  (make-named-object-custom-write type-name))

(define (default-object-name-property descriptor)
  (object-type-name-field-position (object-descriptor-type descriptor)))

(define (make-object-field-accessor descriptor position)
  (define type (object-descriptor-type descriptor))
  (define accessor (object-descriptor-accessor descriptor))
  (define fields (object-type-fields type))
  (define field (keyset-ref fields position))
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
