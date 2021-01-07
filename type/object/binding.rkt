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
  [object-binding-fields
   (-> object-binding? (vectorof identifier? #:immutable #t))]
  [object-binding-field-accessors
   (-> object-binding? (vectorof identifier? #:immutable #t))]
  [object-binding-private-fields
   (-> object-binding? (vectorof identifier? #:immutable #t))]
  [object-binding-private-accessors
   (-> object-binding? (vectorof identifier? #:immutable #t))]
  [object-binding-name-field (-> object-binding? identifier?)]
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
         #:fields (sequence/c identifier?)
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
   fields
   field-accessors)
  #:omit-define-syntaxes
  #:constructor-name constructor:object-binding)

(define (object-binding
         #:type type
         #:descriptor descriptor
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor
         #:fields fields
         #:field-accessors field-accessors)
  (define field-vector
    (vector->immutable-vector (for/vector ([field fields]) field)))
  (define field-accessor-vector
    (vector->immutable-vector
     (for/vector ([field-accessor field-accessors]) field-accessor)))
  (constructor:object-binding
   type
   descriptor
   predicate
   constructor
   accessor
   field-vector
   field-accessor-vector))

(define (object-binding-private-fields binding)
  (define fields (object-binding-fields binding))
  (define type (object-binding-type binding))
  (define name-field-position (object-type-name-field-position type))
  (define before (subsequence fields 0 name-field-position))
  (define after (subsequence fields (add1 name-field-position)))
  (vector->immutable-vector
   (for/vector ([field (sequence-append before after)]) field)))

(define (object-binding-private-accessors binding)
  (define accessors (object-binding-field-accessors binding))
  (define type (object-binding-type binding))
  (define name-field-position (object-type-name-field-position type))
  (define before (subsequence accessors 0 name-field-position))
  (define after (subsequence accessors (add1 name-field-position)))
  (vector->immutable-vector
   (for/vector ([accessor (sequence-append before after)]) accessor)))

(define (object-binding-name-field binding)
  (define fields (object-binding-fields binding))
  (define type (object-binding-type binding))
  (define name-field-position (object-type-name-field-position type))
  (vector-ref fields name-field-position))

(define (object-binding-name-accessor binding)
  (define accessors (object-binding-field-accessors binding))
  (define type (object-binding-type binding))
  (define name-field-position (object-type-name-field-position type))
  (vector-ref accessors name-field-position))

(define-syntax-class object-id
  #:attributes
  (type
   binding
   name
   descriptor
   predicate
   constructor
   accessor
   [field 1]
   [field-name 1]
   [field-keyword 1]
   [field-accessor 1]
   [private-field 1]
   [private-field-name 1]
   [private-field-keyword 1]
   [private-accessor 1]
   name-field
   name-field-name
   name-field-keyword
   name-accessor)

  (pattern binding-id
    #:declare binding-id
    (static object-binding? "a static object-binding? value")

    #:attr binding (attribute binding-id.value)
    #:attr type (object-binding-type (attribute binding))
    #:with name #`'#,(object-type-name (attribute type))
    #:with descriptor (object-binding-descriptor (attribute binding))
    #:with predicate (object-binding-predicate (attribute binding))
    #:with constructor (object-binding-constructor (attribute binding))
    #:with accessor (object-binding-accessor (attribute binding))

    #:with (field ...)
    (sequence->list (object-binding-fields (attribute binding)))

    #:with (field-name ...)
    (for/list ([field-kw (in-keyset (object-type-fields (attribute type)))])
      #`'#,(string->symbol (keyword->string field-kw)))


    #:with (field-keyword ...)
    (sequence->list (object-type-fields (attribute type)))

    #:with (field-accessor ...)
    (sequence->list (object-binding-field-accessors (attribute binding)))

    #:with (private-field ...)
    (sequence->list (object-binding-private-fields (attribute binding)))

    #:with (private-field-name ...)
    (for/list
        ([field-kw (in-keyset (object-type-private-fields (attribute type)))])
      #`'#,(string->symbol (keyword->string field-kw)))

    #:with (private-field-keyword ...)
    (sequence->list (object-type-private-fields (attribute type)))

    #:with (private-accessor ...)
    (sequence->list (object-binding-private-accessors (attribute binding)))

    #:with name-field (object-binding-name-field (attribute binding))

    #:with name-field-name
    #`'#,(string->symbol
          (keyword->string (object-type-name-field (attribute type))))

    #:with name-field-keyword (object-type-name-field (attribute type))
    #:with name-accessor (object-binding-name-accessor (attribute binding))))
