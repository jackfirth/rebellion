#lang racket/base

(require racket/contract/base)

(provide
 define-singleton-type
 (contract-out
  [singleton-type
   (->* (interned-symbol?) (#:predicate-name (or/c interned-symbol? #f))
        singleton-type?)]
  [singleton-type? (-> any/c boolean?)]
  [singleton-type-name (-> singleton-type? interned-symbol?)]
  [singleton-type-predicate-name (-> singleton-type? interned-symbol?)]
  [singleton-descriptor? (-> any/c boolean?)]
  [singleton-type-make-implementation
   (->* (singleton-type?)
        (#:inspector inspector?
         #:property-maker (-> uninitialized-singleton-descriptor?
                              (listof (cons/c struct-type-property? any/c))))
        initialized-singleton-descriptor?)]
  [initialized-singleton-descriptor? (-> any/c boolean?)]
  [uninitialized-singleton-descriptor? (-> any/c boolean?)]
  [singleton-descriptor-instance (-> initialized-singleton-descriptor? any/c)]
  [singleton-descriptor-predicate
   (-> singleton-descriptor? (-> any/c boolean?))]))

(require (for-syntax racket/base
                     racket/syntax)
         rebellion/custom-write
         rebellion/custom-write/tuple
         rebellion/equal+hash
         rebellion/symbol
         rebellion/tuple-type
         rebellion/tuple-type-definition
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define-tuple-type singleton-type (name predicate-name)
  #:constructor constructor:singleton-type)

(define (singleton-type name #:predicate-name [pred-name #f])
  (constructor:singleton-type name pred-name))

(define (make-singleton-properties descriptor)
  (define accessor (tuple-descriptor-accessor descriptor))
    (list (cons prop:object-name
                (λ (this) (singleton-type-name (accessor this 0))))
          (cons prop:custom-write
                (make-tuple-named-object-custom-write descriptor))))

(define-tuple-type initialized-singleton-descriptor
  (type instance predicate)
  #:constructor constructor:initialized-singleton-descriptor
  #:property-maker make-singleton-properties)

(define (initialized-singleton-descriptor #:type type
                                          #:instance instance
                                          #:predicate predicate)
  (constructor:initialized-singleton-descriptor type instance predicate))

(define-tuple-type uninitialized-singleton-descriptor
  (type predicate)
  #:constructor constructor:uninitialized-singleton-descriptor
  #:property-maker make-singleton-properties)

(define (uninitialized-singleton-descriptor #:type type #:predicate predicate)
  (constructor:uninitialized-singleton-descriptor type predicate))

(define (singleton-descriptor? v)
  (or (initialized-singleton-descriptor? v)
      (uninitialized-singleton-descriptor? v)))

(define (singleton-type-make-implementation
         type
         #:inspector [inspector (current-inspector)]
         #:property-maker [prop-maker make-default-singleton-properties])
  (define (make-tuple-props tuple-descriptor)
    (define predicate (tuple-descriptor-predicate tuple-descriptor))
    (prop-maker
     (uninitialized-singleton-descriptor #:type type #:predicate predicate)))
  (define type/tuple
    (tuple-type (singleton-type-name type) 0
                #:predicate-name (singleton-type-predicate-name type)))
  (define descriptor
    (tuple-type-make-implementation type/tuple
                                    #:property-maker make-tuple-props))
  (define instance ((tuple-descriptor-constructor descriptor)))
  (define pred (tuple-descriptor-predicate descriptor))
  (initialized-singleton-descriptor #:type type
                                    #:instance instance
                                    #:predicate pred))

(define (make-default-singleton-properties descriptor)
  (define name (singleton-type-name (singleton-descriptor-type descriptor)))
  (list (cons prop:object-name (λ (_) name))
        (cons prop:equal+hash (make-singleton-equal+hash))
        (cons prop:custom-write (make-singleton-custom-write name))))

;@------------------------------------------------------------------------------

(define (singleton-descriptor-case descriptor #:initialized f #:uninitialized g)
  (if (initialized-singleton-descriptor? descriptor)
      (f descriptor)
      (g descriptor)))

(define (singleton-descriptor-type descriptor)
  (singleton-descriptor-case
   descriptor
   #:initialized initialized-singleton-descriptor-type
   #:uninitialized uninitialized-singleton-descriptor-type))

(define (singleton-descriptor-predicate descriptor)
  (singleton-descriptor-case
   descriptor
   #:initialized initialized-singleton-descriptor-predicate
   #:uninitialized uninitialized-singleton-descriptor-predicate))

(define (singleton-descriptor-instance descriptor)
  (initialized-singleton-descriptor-instance descriptor))

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class singleton-id
    #:attributes (default-name
                  default-predicate-name
                  default-descriptor-name
                  default-type-name)
    (pattern id:id
      #:do [(define (format-singleton-id fmt)
              (format-id #'id fmt #'id #:source #'id #:props #'id))]
      #:with default-name #'id
      #:with default-predicate-name (format-singleton-id "~a?")
      #:with default-descriptor-name (format-singleton-id "descriptor:~a")
      #:with default-type-name (format-singleton-id "type:~a"))))

(define-simple-macro
  (define-singleton-type id:singleton-id
    (~alt (~optional (~seq #:name name:id)
                     #:name "#:name option"
                     #:defaults ([name #'id.default-name]))
          
          (~optional (~seq #:predicate-name predicate:id)
                     #:name "#:predicate-name option"
                     #:defaults ([predicate #'id.default-predicate-name]))
          
          (~optional (~seq #:descriptor-name descriptor:id)
                     #:name "#:descriptor-name option"
                     #:defaults ([descriptor #'id.default-descriptor-name]))
          
          (~optional (~seq #:type-representation-name type:id)
                     #:name "#:type-representation-name option"
                     #:defaults ([type #'id.default-type-name]))
          
          (~optional (~seq #:inspector inspector:expr)
                     #:name "#:inspector option"
                     #:defaults ([inspector #'(current-inspector)]))
          
          (~optional (~seq #:property-maker prop-maker:expr)
                     #:name "#:property-maker option"
                     #:defaults
                     ([prop-maker #'make-default-singleton-properties])))
    ...)
  (begin
    (define type (singleton-type 'name #:predicate-name 'predicate))
    (define descriptor
      (singleton-type-make-implementation
       type #:inspector inspector #:property-maker prop-maker))
    (define name (singleton-descriptor-instance descriptor))
    (define predicate (singleton-descriptor-predicate descriptor))))
