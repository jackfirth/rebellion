#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [object-type
   (->* (interned-symbol? keyset?)
        (#:name-field keyword?
         #:constructor-name (or/c interned-symbol? #f)
         #:accessor-name (or/c interned-symbol? #f)
         #:predicate-name (or/c interned-symbol? #f))
        object-type?)]
  [object-type? predicate/c]
  [object-type-name (-> object-type? interned-symbol?)]
  [object-type-fields (-> object-type? keyset?)]
  [object-type-private-fields (-> object-type? keyset?)]
  [object-type-name-field (-> object-type? keyword?)]
  [object-type-name-field-position (-> object-type? natural?)]
  [object-type-constructor-name (-> object-type? interned-symbol?)]
  [object-type-predicate-name (-> object-type? interned-symbol?)]
  [object-type-accessor-name (-> object-type? interned-symbol?)]
  [object-type-size (-> object-type? natural?)]))

(require racket/math
         rebellion/base/symbol
         rebellion/collection/keyset/low-dependency
         rebellion/type/private/naming
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type object-type
  (name
   fields
   name-field-position
   constructor-name
   predicate-name
   accessor-name)
  #:omit-root-binding)

(define (object-type name fields
                     #:name-field [name-field '#:name]
                     #:constructor-name [constructor-name #f]
                     #:accessor-name [accessor-name #f]
                     #:predicate-name [predicate-name #f])
  (define all-fields (keyset-add fields name-field))
  (constructor:object-type
   #:name name
   #:fields all-fields
   #:name-field-position (keyset-index-of all-fields name-field)
   #:constructor-name
   (or constructor-name (default-opaque-constructor-name name))
   #:accessor-name (or accessor-name (default-accessor-name name))
   #:predicate-name (or predicate-name (default-predicate-name name))))

(define (object-type-name-field type)
  (keyset-ref (object-type-fields type) (object-type-name-field-position type)))

(define (object-type-size type) (keyset-size (object-type-fields type)))

(define (object-type-private-fields type)
  (define fields (object-type-fields type))
  (keyset-remove fields (object-type-name-field type)))
