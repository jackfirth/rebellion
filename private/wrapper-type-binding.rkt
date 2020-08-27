#lang racket/base

(require racket/contract/base)

(provide
 wrapper-id
 (contract-out
  [wrapper-binding? predicate/c]
  [wrapper-binding-type (-> wrapper-binding? wrapper-type?)]
  [wrapper-binding-descriptor (-> wrapper-binding? identifier?)]
  [wrapper-binding-predicate (-> wrapper-binding? identifier?)]
  [wrapper-binding-constructor (-> wrapper-binding? identifier?)]
  [wrapper-binding-accessor (-> wrapper-binding? identifier?)]))

(module+ private-constructor
  (provide
   (contract-out
    [wrapper-binding
     (-> #:type wrapper-type?
         #:descriptor identifier?
         #:predicate identifier?
         #:constructor identifier?
         #:accessor identifier?
         #:pattern identifier?
         #:macro (-> syntax? syntax?)
         wrapper-binding?)])))

(require (for-template racket/base
                       racket/match)
         racket/sequence
         racket/syntax
         rebellion/type/wrapper/base
         syntax/parse)

;@------------------------------------------------------------------------------

(struct wrapper-binding
  (type descriptor predicate constructor accessor pattern macro)
  #:omit-define-syntaxes
  #:constructor-name constructor:wrapper-binding

  #:property prop:match-expander
  (λ (this stx)
    (define/with-syntax pattern (wrapper-binding-pattern this))
    (syntax-parse stx #:track-literals
      [(_ . body) (quasisyntax/loc stx (pattern . body))]))

  #:property prop:procedure (λ (this stx) ((wrapper-binding-macro this) stx)))

(define (wrapper-binding
         #:type type
         #:descriptor descriptor
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor
         #:pattern pattern
         #:macro macro)
  (constructor:wrapper-binding
   type descriptor predicate constructor accessor pattern macro))

(define-syntax-class wrapper-id
  #:attributes (type binding name descriptor predicate constructor accessor)

  (pattern binding-id
    #:declare binding-id
    (static wrapper-binding? "a static wrapper-binding? value")

    #:attr binding (attribute binding-id.value)
    #:attr type (wrapper-binding-type (attribute binding))
    #:with name #`'#,(wrapper-type-name (attribute type))
    #:with descriptor (wrapper-binding-descriptor (attribute binding))
    #:with predicate (wrapper-binding-predicate (attribute binding))
    #:with constructor (wrapper-binding-constructor (attribute binding))
    #:with accessor (wrapper-binding-accessor (attribute binding))))
