#lang racket/base

(provide define-tuple-type)

(require (for-syntax racket/base
                     racket/syntax)
         rebellion/type/tuple/base
         rebellion/type/tuple/descriptor
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-tuple-type id:id (field:id ...)
    (~alt (~optional (~seq #:constructor-name constructor:id)
                     #:defaults ([constructor #'id]))
          (~optional (~seq #:predicate-name predicate:id)
                     #:defaults ([predicate
                                  (format-id #'id "~a?" (syntax-e #'id))]))
          (~optional (~seq #:property-maker property-maker:expr)
                     #:defaults ([property-maker
                                  #'default-tuple-properties])))
    ...)
  #:do [(define size (length (syntax->list #'(field ...))))]
  #:with quoted-size #`(quote #,size)
  #:with (field-accessor ...)
  (map (λ (field-id)
         (format-id field-id "~a-~a" (syntax-e #'id) (syntax-e field-id)))
       (syntax->list #'(field ...)))
  #:with (field-position ...) (build-list size (λ (n) #`(quote #,n)))
  (begin
    (define descriptor
      (make-tuple-implementation
       (tuple-type 'id quoted-size
                   #:constructor-name 'constructor
                   #:predicate-name 'predicate)
       #:property-maker property-maker))
    (define constructor (tuple-descriptor-constructor descriptor))
    (define predicate (tuple-descriptor-predicate descriptor))
    (define field-accessor
      (make-tuple-field-accessor descriptor field-position 'field))
    ...))
