#lang racket/base

(require racket/contract/base)

(provide
 tuple-id
 (contract-out
  [tuple-binding? predicate/c]
  [tuple-binding-type (-> tuple-binding? tuple-type?)]
  [tuple-binding-descriptor (-> tuple-binding? identifier?)]
  [tuple-binding-predicate (-> tuple-binding? identifier?)]
  [tuple-binding-constructor (-> tuple-binding? identifier?)]
  [tuple-binding-accessor (-> tuple-binding? identifier?)]
  [tuple-binding-fields
   (-> tuple-binding? (vectorof identifier? #:immutable #t))]
  [tuple-binding-field-accessors
   (-> tuple-binding? (vectorof identifier? #:immutable #t))]))

(module+ private-constructor
  (provide
   (contract-out
    [tuple-binding
     (-> #:type tuple-type?
         #:descriptor identifier?
         #:predicate identifier?
         #:constructor identifier?
         #:accessor identifier?
         #:fields (sequence/c identifier?)
         #:field-accessors (sequence/c identifier?)
         #:pattern identifier?
         #:macro (-> syntax? syntax?)
         tuple-binding?)])))

(require (for-template racket/base
                       racket/match)
         racket/sequence
         racket/syntax
         rebellion/type/tuple/base
         syntax/parse)

;@------------------------------------------------------------------------------

(struct tuple-binding
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
  #:constructor-name constructor:tuple-binding

  #:property prop:match-expander
  (λ (this stx)
    (define/with-syntax pattern (tuple-binding-pattern this))
    (syntax-parse stx #:track-literals
      [(_ . body) (quasisyntax/loc stx (pattern . body))]))

  #:property prop:procedure (λ (this stx) ((tuple-binding-macro this) stx)))

(define (tuple-binding
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
  (constructor:tuple-binding
   type
   descriptor
   predicate
   constructor
   accessor
   field-vector
   field-accessor-vector
   pattern
   macro))

(define-syntax-class tuple-id
  #:attributes
  (type
   name
   descriptor
   predicate
   constructor
   accessor
   [field 1]
   [field-name 1]
   [field-accessor 1])

  (pattern binding
    #:declare binding (static tuple-binding? "a static tuple-binding? value")
    #:attr type (tuple-binding-type (attribute binding.value))
    #:with name #`'#,(tuple-type-name (attribute type))
    #:with descriptor (tuple-binding-descriptor (attribute binding.value))
    #:with predicate (tuple-binding-predicate (attribute binding.value))
    #:with constructor (tuple-binding-constructor (attribute binding.value))
    #:with accessor (tuple-binding-accessor (attribute binding.value))

    #:with (field ...)
    (sequence->list (tuple-binding-fields (attribute binding.value)))
    
    #:with (field-name ...)
    (for/list ([field-name (in-vector (tuple-type-fields (attribute type)))])
      #`'#,field-name)

    #:with (field-accessor ...)
    (sequence->list (tuple-binding-field-accessors (attribute binding.value)))))
