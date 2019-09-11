#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [reference-type
   (->* (interned-symbol? keyset?)
        (#:object-name-field natural?
         #:constructor-name (or/c interned-symbol? #f)
         #:accessor-name (or/c interned-symbol? #f)
         #:predicate-name (or/c interned-symbol? #f))
        reference-type?)]
  [reference-type? predicate/c]
  [reference-type-name (-> reference-type? interned-symbol?)]
  [reference-type-fields (-> reference-type? keyset?)]
  [reference-type-object-name-field (-> reference-type? natural?)]
  [reference-type-constructor-name (-> reference-type? interned-symbol?)]
  [reference-type-predicate-name (-> reference-type? interned-symbol?)]
  [reference-type-accessor-name (-> reference-type? interned-symbol?)]
  [reference-type-size (-> reference-type? natural?)]))

(require racket/math
         racket/syntax
         rebellion/base/symbol
         rebellion/collection/keyset/low-dependency
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type reference-type
  (name fields object-name-field constructor-name predicate-name accessor-name)
  #:constructor-name constructor:reference-type)

(define (reference-type name fields
                        #:object-name-field [name-field
                                             (keyset-index-of fields '#:name)]
                        #:constructor-name [constructor-name #f]
                        #:accessor-name [accessor-name #f]
                        #:predicate-name [predicate-name #f])
  (constructor:reference-type
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

(define (reference-type-size type) (keyset-size (reference-type-fields type)))
