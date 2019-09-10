#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [wrapper-binding (-> #:type wrapper-type?
                       #:descriptor-id identifier?
                       #:predicate-id identifier?
                       #:constructor-id identifier?
                       #:accessor-id identifier?
                       #:target-id identifier?
                       wrapper-binding?)]
  [wrapper-binding? predicate/c]
  [wrapper-binding-type (-> wrapper-binding? wrapper-type?)]
  [wrapper-binding-descriptor-id (-> wrapper-binding? identifier?)]
  [wrapper-binding-predicate-id (-> wrapper-binding? identifier?)]
  [wrapper-binding-constructor-id (-> wrapper-binding? identifier?)]
  [wrapper-binding-accessor-id (-> wrapper-binding? identifier?)]))

(require (for-template racket/match)
         rebellion/collection/keyset
         rebellion/custom-write
         rebellion/type/record
         rebellion/type/wrapper/base
         syntax/parse)

;@------------------------------------------------------------------------------

(define (make-wrapper-binding-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define accessor (record-descriptor-accessor descriptor))
  (define type-field (keyset-index-of (record-type-fields type) '#:type))
  (define (object-name this) (wrapper-type-name (accessor this type-field)))
  (define type-name (record-type-name type))
  (define custom-write (make-named-object-custom-write type-name))
  (define equal+hash (make-record-equal+hash descriptor))
  (define predicate-id-field
    (keyset-index-of (record-type-fields type) '#:predicate-id))
  (define accessor-id-field
    (keyset-index-of (record-type-fields type) '#:accessor-id))
  (define (match-expander this stx)
    (syntax-parse stx
      [(_ value-pat)
       #:with predicate-id (accessor this predicate-id-field)
       #:with accessor-id (accessor this accessor-id-field)
       #'(? predicate-id
            (app accessor-id
                 value-pat))]))
  (list (cons prop:custom-write custom-write)
        (cons prop:object-name object-name)
        (cons prop:equal+hash equal+hash)
        (cons prop:match-expander match-expander)))

(define-record-type wrapper-binding
  (type descriptor-id predicate-id constructor-id accessor-id target-id)
  #:property-maker make-wrapper-binding-properties)

(define-syntax-class wrapper-id
  #:attributes (binding type descriptor predicate constructor accessor)
  (pattern (~var id (static wrapper-binding? "wrapper type binding"))
    #:attr binding (attribute id.value)
    #:attr type (wrapper-binding-type (attribute binding))
    #:with descriptor (wrapper-binding-descriptor-id (attribute binding))
    #:with predicate (wrapper-binding-predicate-id (attribute binding))
    #:with constructor (wrapper-binding-constructor-id (attribute binding))
    #:with accessor (wrapper-binding-accessor-id (attribute binding))))
