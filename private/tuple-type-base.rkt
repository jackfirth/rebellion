#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [tuple-type
   (->* (interned-symbol? (sequence/c interned-symbol?))
        (#:predicate-name (or/c interned-symbol? #f)
         #:constructor-name (or/c interned-symbol? #f)
         #:accessor-name (or/c interned-symbol? #f))
        tuple-type?)]
  [tuple-type? (-> any/c boolean?)]
  [tuple-type-name (-> tuple-type? interned-symbol?)]
  [tuple-type-fields
   (-> tuple-type? (vectorof interned-symbol? #:immutable #t))]
  [tuple-type-accessor-name (-> tuple-type? interned-symbol?)]
  [tuple-type-constructor-name (-> tuple-type? interned-symbol?)]
  [tuple-type-predicate-name (-> tuple-type? interned-symbol?)]
  [tuple-type-size (-> tuple-type? natural?)]))

(require racket/list
         racket/math
         racket/sequence
         racket/syntax
         rebellion/base/symbol)

;@------------------------------------------------------------------------------

(struct tuple-type (name fields predicate-name constructor-name accessor-name)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:tuple-type)

(define (tuple-type
         name
         fields
         #:predicate-name [predicate-name* #f]
         #:constructor-name [constructor-name* #f]
         #:accessor-name [accessor-name* #f])
  (define field-vector
    (vector->immutable-vector (for/vector ([field fields]) field)))
  (check-field-names-unique field-vector)
  (define predicate-name
    (or predicate-name* (default-tuple-predicate-name name)))
  (define constructor-name (or constructor-name* name))
  (define accessor-name (or accessor-name* (default-tuple-accessor-name name)))
  (constructor:tuple-type
   name field-vector predicate-name constructor-name accessor-name))

(define (tuple-type-size type)
  (vector-length (tuple-type-fields type)))

(define (check-field-names-unique names)
  (define duplicate (check-duplicates (vector->list names)))
  (when duplicate
    (raise-arguments-error
     'tuple-type
     "duplicate field names are not allowed in tuple types"
     "duplicate name" duplicate)))

(define (default-tuple-predicate-name type-name)
  (format-symbol "~a?" type-name))

(define (default-tuple-accessor-name type-name)
  (format-symbol "~a-ref" type-name))
