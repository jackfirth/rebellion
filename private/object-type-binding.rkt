#lang racket/base

(require racket/contract/base)

(provide
 object-id
 (contract-out
  [object-binding? predicate/c]
  [object-binding-type (-> object-binding? object-type?)]
  [object-binding-descriptor (-> object-binding? identifier?)]
  [object-binding-predicate (-> object-binding? identifier?)]
  [object-binding-constructor (-> object-binding? identifier?)]
  [object-binding-accessor (-> object-binding? identifier?)]
  [object-binding-field-accessors
   (-> object-binding? (vectorof identifier? #:immutable #t))]
  [object-binding-private-accessors
   (-> object-binding? (vectorof identifier? #:immutable #t))]
  [object-binding-name-accessor (-> object-binding? identifier?)]))

(module+ private-constructor
  (provide
   (contract-out
    [object-binding
     (-> #:type object-type?
         #:descriptor identifier?
         #:predicate identifier?
         #:constructor identifier?
         #:accessor identifier?
         #:field-accessors (sequence/c identifier?)
         object-binding?)])))

(require (for-template racket/base
                       racket/match)
         racket/sequence
         racket/syntax
         rebellion/collection/keyset/low-dependency
         rebellion/private/subsequence
         rebellion/type/object/base
         syntax/parse)

;@------------------------------------------------------------------------------

(struct object-binding
  (type
   descriptor
   predicate
   constructor
   accessor
   field-accessors)
  #:omit-define-syntaxes
  #:constructor-name constructor:object-binding)

(define (object-binding
         #:type type
         #:descriptor descriptor
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor
         #:field-accessors field-accessors)
  (define field-accessor-vector
    (vector->immutable-vector
     (for/vector ([field-accessor field-accessors]) field-accessor)))
  (constructor:object-binding
   type
   descriptor
   predicate
   constructor
   accessor
   field-accessor-vector))

(define (object-binding-private-accessors binding)
  (define accessors (object-binding-field-accessors binding))
  (define type (object-binding-type binding))
  (define name-field-position (object-type-name-field-position type))
  (define before (subsequence accessors 0 name-field-position))
  (define after (subsequence accessors (add1 name-field-position)))
  (vector->immutable-vector
   (for/vector ([accessor (sequence-append before after)]) accessor)))

(define (object-binding-name-accessor binding)
  (define accessors (object-binding-field-accessors binding))
  (define type (object-binding-type binding))
  (define name-field-position (object-type-name-field-position type))
  (vector-ref accessors name-field-position))

(define-syntax-class object-id
  #:attributes
  (type
   name
   [field-name 1]
   [private-field-name 1]
   name-field-name
   descriptor
   predicate
   constructor
   accessor
   [field-accessor 1]
   [private-accessor 1]
   name-accessor)

  (pattern binding
    #:declare binding (static object-binding? "a static object-binding? value")
    #:cut
    #:attr type (object-binding-type (attribute binding.value))
    #:with name #`'#,(object-type-name (attribute type))
    #:with (field-name ...)
    (for/list ([field-kw (in-keyset (object-type-fields (attribute type)))])
      #`'#,(string->symbol (keyword->string field-kw)))
    #:with (private-field-name ...)
    (for/list
        ([field-kw (in-keyset (object-type-private-fields (attribute type)))])
      #`'#,(string->symbol (keyword->string field-kw)))
    #:with name-field-name
    #`'#,(string->symbol
          (keyword->string (object-type-name-field (attribute type))))
    #:with descriptor (object-binding-descriptor (attribute binding.value))
    #:with predicate (object-binding-predicate (attribute binding.value))
    #:with constructor (object-binding-constructor (attribute binding.value))
    #:with accessor (object-binding-accessor (attribute binding.value))
    #:with (field-accessor ...)
    (sequence->list
     (object-binding-field-accessors (attribute binding.value)))
    #:with (private-accessor ...)
    (sequence->list
     (object-binding-private-accessors (attribute binding.value)))
    #:with name-accessor
    (object-binding-name-accessor (attribute binding.value))))
