#lang racket/base

(require racket/contract/base)

(provide
 record-id
 (contract-out
  [record-binding? predicate/c]
  [record-binding-type (-> record-binding? record-type?)]
  [record-binding-descriptor (-> record-binding? identifier?)]
  [record-binding-predicate (-> record-binding? identifier?)]
  [record-binding-constructor (-> record-binding? identifier?)]
  [record-binding-accessor (-> record-binding? identifier?)]
  [record-binding-fields
   (-> record-binding? (vectorof identifier? #:immutable #t))]
  [record-binding-field-accessors
   (-> record-binding? (vectorof identifier? #:immutable #t))]))

(module+ private-constructor
  (provide
   (contract-out
    [record-binding
     (-> #:type record-type?
         #:descriptor identifier?
         #:predicate identifier?
         #:constructor identifier?
         #:accessor identifier?
         #:fields (sequence/c identifier?)
         #:field-accessors (sequence/c identifier?)
         #:pattern identifier?
         #:macro (-> syntax? syntax?)
         record-binding?)])))

(require (for-template racket/base
                       racket/match)
         racket/sequence
         racket/syntax
         rebellion/collection/keyset/low-dependency
         rebellion/type/record/base
         syntax/parse)

;@------------------------------------------------------------------------------

(struct record-binding
  (type
   descriptor
   predicate
   constructor
   accessor
   fields
   field-accessors
   pattern
   macro)
  #:omit-define-syntaxes
  #:constructor-name constructor:record-binding

  #:property prop:match-expander
  (λ (this stx)
    (define/with-syntax pattern (record-binding-pattern this))
    (syntax-parse stx #:track-literals
      [(_ . body) (quasisyntax/loc stx (pattern . body))]))

  #:property prop:procedure (λ (this stx) ((record-binding-macro this) stx)))

(define (record-binding
         #:type type
         #:descriptor descriptor
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor
         #:fields fields
         #:field-accessors field-accessors
         #:pattern pattern
         #:macro macro)
  (define field-vector
    (vector->immutable-vector (for/vector ([field fields]) field)))
  (define field-accessor-vector
    (vector->immutable-vector
     (for/vector ([field-accessor field-accessors]) field-accessor)))
  (constructor:record-binding
   type
   descriptor
   predicate
   constructor
   accessor
   field-vector
   field-accessor-vector
   pattern
   macro))

(define-syntax-class record-id
  #:attributes
  (type
   name
   descriptor
   predicate
   constructor
   accessor
   [field 1]
   [field-name 1]
   [field-keyword 1]
   [field-accessor 1])

  (pattern binding
    #:declare binding (static record-binding? "a static record-binding? value")
    #:attr type (record-binding-type (attribute binding.value))
    #:with name #`'#,(record-type-name (attribute type))
    #:with descriptor (record-binding-descriptor (attribute binding.value))
    #:with predicate (record-binding-predicate (attribute binding.value))
    #:with constructor (record-binding-constructor (attribute binding.value))
    #:with accessor (record-binding-accessor (attribute binding.value))

    #:with (field ...)
    (sequence->list (record-binding-fields (attribute binding.value)))

    #:with (field-name ...)
    (for/list ([field-kw (in-keyset (record-type-fields (attribute type)))])
      #`'#,(string->symbol (keyword->string field-kw)))

    #:with (field-keyword ...)
    (sequence->list (record-type-fields (attribute type)))
    
    #:with (field-accessor ...)
    (sequence->list
     (record-binding-field-accessors (attribute binding.value)))))
