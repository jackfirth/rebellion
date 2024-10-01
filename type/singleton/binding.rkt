#lang racket/base

(require racket/contract/base)

(provide
 singleton-id
 (contract-out
  [singleton-binding? (-> any/c boolean?)]
  [singleton-binding-type (-> singleton-binding? singleton-type?)]
  [singleton-binding-descriptor (-> singleton-binding? identifier?)]
  [singleton-binding-predicate (-> singleton-binding? identifier?)]
  [singleton-binding-instance (-> singleton-binding? identifier?)]))

(module+ private-constructor
  (provide
   (contract-out
    [singleton-binding
     (-> #:type singleton-type?
         #:descriptor identifier?
         #:predicate identifier?
         #:instance identifier?
         #:macro (-> syntax? syntax?)
         singleton-binding?)])))

(require (for-template racket/base)
         racket/sequence
         racket/syntax
         rebellion/type/singleton/base
         syntax/parse)

;@------------------------------------------------------------------------------

(struct singleton-binding
  (type descriptor predicate instance macro)
  #:omit-define-syntaxes
  #:constructor-name constructor:singleton-binding
  #:property prop:procedure (λ (this stx) ((singleton-binding-macro this) stx)))

(define (singleton-binding
         #:type type
         #:descriptor descriptor
         #:predicate predicate
         #:instance instance
         #:macro macro)
  (constructor:singleton-binding type descriptor predicate instance macro))

(define-syntax-class singleton-id
  #:attributes (type binding name descriptor predicate instance)

  (pattern binding-id
    #:declare binding-id
    (static singleton-binding? "a static singleton-binding? value")

    #:attr binding (attribute binding-id.value)
    #:attr type (singleton-binding-type (attribute binding))
    #:with name #`'#,(singleton-type-name (attribute type))
    #:with descriptor (singleton-binding-descriptor (attribute binding))
    #:with predicate (singleton-binding-predicate (attribute binding))
    #:with instance (singleton-binding-instance (attribute binding))))
