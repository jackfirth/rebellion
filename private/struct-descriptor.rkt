#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [struct-descriptor
   (-> #:type struct-type?
       #:constructor struct-constructor-procedure?
       #:predicate struct-predicate-procedure?
       #:accessor struct-accessor-procedure?
       #:mutator struct-mutator-procedure?
       struct-descriptor?)]
  [struct-descriptor? (-> any/c boolean?)]
  [struct-descriptor-type (-> struct-descriptor? struct-type?)]
  [struct-descriptor-constructor
   (-> struct-descriptor? struct-constructor-procedure?)]
  [struct-descriptor-predicate
   (-> struct-descriptor? struct-predicate-procedure?)]
  [struct-descriptor-accessor
   (-> struct-descriptor? struct-accessor-procedure?)]
  [struct-descriptor-mutator
   (-> struct-descriptor? struct-mutator-procedure?)]
  [make-struct-type/descriptor
   (->* (#:name symbol?)
        (#:mutable-fields natural?
         #:immutable-fields natural?
         #:auto-fields natural?
         #:super-type (or/c struct-type? #f)
         #:auto-field-value any/c
         #:properties (listof (cons/c struct-type-property? any/c))
         #:inspector (or/c inspector? #f 'prefab)
         #:guard (or/c procedure? #f)
         #:constructor-name (or/c symbol? #f))
        struct-descriptor?)]))

(require racket/list
         racket/math)

;@------------------------------------------------------------------------------

(struct struct-descriptor (type constructor predicate accessor mutator)
  #:constructor-name plain-struct-descriptor
  #:omit-define-syntaxes)

(define (struct-descriptor #:type type
                           #:constructor constructor
                           #:predicate predicate
                           #:accessor accessor
                           #:mutator mutator)
  (plain-struct-descriptor type constructor predicate accessor mutator))


(define (make-struct-type/descriptor #:name name
                                     #:mutable-fields [mutable-fields 0]
                                     #:immutable-fields [immutable-fields 0]
                                     #:auto-fields [auto-fields 0]
                                     #:super-type [super-type #f]
                                     #:auto-field-value [auto-field-value #f]
                                     #:properties [properties empty]
                                     #:inspector [inspector (current-inspector)]
                                     #:guard [guard #f]
                                     #:constructor-name [constructor-name #f])
  (define immutables
    (range mutable-fields (+ mutable-fields immutable-fields)))
  (define-values (type constructor predicate accessor mutator)
    (make-struct-type name
                      super-type
                      (+ mutable-fields immutable-fields)
                      auto-fields
                      auto-field-value
                      properties
                      inspector
                      #f
                      immutables
                      guard
                      constructor-name))
  (struct-descriptor #:type type
                     #:constructor constructor
                     #:predicate predicate
                     #:accessor accessor
                     #:mutator mutator))
