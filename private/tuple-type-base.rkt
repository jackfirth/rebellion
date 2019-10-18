#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [tuple-type
   (->* (interned-symbol? natural?)
        (#:predicate-name (or/c interned-symbol? #f)
         #:constructor-name (or/c interned-symbol? #f)
         #:accessor-name (or/c interned-symbol? #f))
        tuple-type?)]
  [tuple-type? (-> any/c boolean?)]
  [tuple-type-accessor-name (-> tuple-type? interned-symbol?)]
  [tuple-type-constructor-name (-> tuple-type? interned-symbol?)]
  [tuple-type-name (-> tuple-type? interned-symbol?)]
  [tuple-type-predicate-name (-> tuple-type? interned-symbol?)]
  [tuple-type-size (-> tuple-type? natural?)]))

(require racket/math
         racket/struct
         racket/syntax
         rebellion/base/symbol
         rebellion/collection/keyset/low-dependency
         rebellion/equal+hash/struct
         rebellion/private/struct-definition-util
         rebellion/type/struct)

;@------------------------------------------------------------------------------

(define (make-struct-constructor-style-custom-write descriptor)
  (define type-name (struct-descriptor-name descriptor))
  (define size
    (+ (struct-descriptor-mutable-fields descriptor)
       (struct-descriptor-immutable-fields descriptor)
       (struct-descriptor-auto-fields descriptor)))
  (define accessor (struct-descriptor-accessor descriptor))
  (make-constructor-style-printer
   (λ (this) type-name)
   (λ (this) (build-list size (λ (pos) (accessor this pos))))))

(define (make-transparent-style-properties descriptor)
  (define equal+hash (make-struct-equal+hash descriptor))
  (define custom-write (make-struct-constructor-style-custom-write descriptor))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

;@------------------------------------------------------------------------------

(define fields:tuple-type
  (keyset #:name
          #:size
          #:predicate-name
          #:constructor-name
          #:accessor-name))

(define descriptor:tuple-type
  (make-struct-implementation
   #:name 'tuple-type
   #:immutable-fields (keyset-size fields:tuple-type)
   #:constructor-name 'constructor:tuple-type
   #:property-maker make-transparent-style-properties))

(define tuple-type? (struct-descriptor-predicate descriptor:tuple-type))

(define constructor:tuple-type
  (struct-descriptor-constructor descriptor:tuple-type))

(define (tuple-type
         name
         size
         #:predicate-name [predicate-name* #f]
         #:constructor-name [constructor-name* #f]
         #:accessor-name [accessor-name* #f])
  (define predicate-name
    (or predicate-name* (default-tuple-predicate-name name)))
  (define constructor-name (or constructor-name* name))
  (define accessor-name (or accessor-name* (default-tuple-accessor-name name)))
  (constructor:tuple-type
   name size predicate-name constructor-name accessor-name))

(define-struct-field-accessors tuple-type
  (name size predicate-name constructor-name accessor-name)
  #:descriptor descriptor:tuple-type)

(define (default-tuple-predicate-name type-name)
  (format-symbol "~a?" type-name))

(define (default-tuple-accessor-name type-name)
  (format-symbol "~a-ref" type-name))
