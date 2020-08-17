#lang racket/base

(require racket/contract/base)

(provide
 enum-id
 (contract-out
  [enum-binding? predicate/c]
  [enum-binding-type (-> enum-binding? enum-type?)]
  [enum-binding-descriptor (-> enum-binding? identifier?)]))

(module+ private-constructor
  (provide
   (contract-out
    [enum-binding
     (-> #:type enum-type?
         #:constants (sequence/c identifier?)
         #:descriptor identifier?
         #:predicate identifier?
         #:discriminator identifier?
         #:selector identifier?
         enum-binding?)])))

(require (for-template racket/base
                       rebellion/type/enum/descriptor)
         racket/sequence
         racket/set
         rebellion/collection/keyset
         rebellion/type/enum/base
         syntax/parse)

;@------------------------------------------------------------------------------

(struct enum-binding
  (type constants descriptor predicate discriminator selector)
  #:omit-define-syntaxes
  #:constructor-name constructor:enum-binding)

(define (enum-binding #:type type
                      #:constants constants
                      #:descriptor descriptor
                      #:predicate predicate
                      #:discriminator discriminator
                      #:selector selector)
  (define constant-set (for/set ([c constants]) c))
  (constructor:enum-binding
   type constant-set descriptor predicate discriminator selector))

(define-syntax-class enum-id
  #:attributes
  (type name
        [constant 1]
        [constant-name 1]
        predicate
        selector
        discriminator
        descriptor)

  (pattern binding
    #:declare binding (static enum-binding? "a static enum binding")
    #:attr type (enum-binding-type (attribute binding.value))
    #:with name #`'#,(enum-type-name (attribute type))
    #:with descriptor (enum-binding-descriptor (attribute binding.value))
    #:with predicate (enum-binding-predicate (attribute binding.value))
    #:with selector (enum-binding-selector (attribute binding.value))
    #:with discriminator (enum-binding-discriminator (attribute binding.value))
    #:with (constant ...)
    (sequence->list (enum-binding-constants (attribute binding.value)))
    #:with (constant-name ...)
    (for/list ([name (in-keyset (enum-type-constants (attribute type)))])
      #`'#,(string->symbol (keyword->string name)))))