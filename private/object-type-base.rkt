#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [object-type
   (->* (interned-symbol? keyset?)
        (#:object-name-field natural?
         #:constructor-name (or/c interned-symbol? #f)
         #:accessor-name (or/c interned-symbol? #f)
         #:predicate-name (or/c interned-symbol? #f))
        object-type?)]
  [object-type? predicate/c]
  [object-type-name (-> object-type? interned-symbol?)]
  [object-type-fields (-> object-type? keyset?)]
  [object-type-object-name-field (-> object-type? natural?)]
  [object-type-constructor-name (-> object-type? interned-symbol?)]
  [object-type-predicate-name (-> object-type? interned-symbol?)]
  [object-type-accessor-name (-> object-type? interned-symbol?)]
  [object-type-size (-> object-type? natural?)]))

(require racket/math
         racket/syntax
         rebellion/base/symbol
         rebellion/collection/keyset/low-dependency
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type object-type
  (name fields object-name-field constructor-name predicate-name accessor-name)
  #:omit-root-binding)

(define (object-type name fields
                     #:object-name-field [name-field
                                          (keyset-index-of fields '#:name)]
                     #:constructor-name [constructor-name #f]
                     #:accessor-name [accessor-name #f]
                     #:predicate-name [predicate-name #f])
  (constructor:object-type
   #:name name
   #:fields fields
   #:object-name-field name-field
   #:constructor-name (or constructor-name (default-constructor-name name))
   #:accessor-name (or accessor-name (default-accessor-name name))
   #:predicate-name (or predicate-name (default-predicate-name name))))

(define (default-constructor-name type-name)
  (format-symbol "make-~a" type-name))

(define (default-accessor-name type-name) (format-symbol "~a-ref" type-name))
(define (default-predicate-name type-name) (format-symbol "~a?" type-name))

(define (object-type-size type) (keyset-size (object-type-fields type)))
