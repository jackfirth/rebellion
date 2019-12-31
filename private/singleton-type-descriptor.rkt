#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [singleton-descriptor? predicate/c]
  [make-singleton-implementation
   (->* (singleton-type?)
        (#:inspector inspector?
         #:property-maker (-> uninitialized-singleton-descriptor?
                              (listof (cons/c struct-type-property? any/c))))
        initialized-singleton-descriptor?)]
  [initialized-singleton-descriptor? predicate/c]
  [uninitialized-singleton-descriptor? predicate/c]
  [singleton-descriptor-instance (-> initialized-singleton-descriptor? any/c)]
  [singleton-descriptor-predicate (-> singleton-descriptor? predicate/c)]
  [default-singleton-properties
   (-> uninitialized-singleton-descriptor?
       (listof (cons/c struct-type-property? any/c)))]))

(require rebellion/custom-write
         rebellion/equal+hash
         rebellion/type/singleton/base
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define (make-singleton-properties descriptor)
  (define accessor (tuple-descriptor-accessor descriptor))
  (define type-name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define (object-name this) (singleton-type-name (accessor this 0)))
  (list (cons prop:object-name object-name)
        (cons prop:custom-write (make-named-object-custom-write type-name))))

(define-tuple-type initialized-singleton-descriptor
  (type instance predicate)
  #:omit-root-binding
  #:property-maker make-singleton-properties)

(define (initialized-singleton-descriptor #:type type
                                          #:instance instance
                                          #:predicate predicate)
  (constructor:initialized-singleton-descriptor type instance predicate))

(define-tuple-type uninitialized-singleton-descriptor
  (type predicate)
  #:omit-root-binding
  #:property-maker make-singleton-properties)

(define (uninitialized-singleton-descriptor #:type type #:predicate predicate)
  (constructor:uninitialized-singleton-descriptor type predicate))

(define (singleton-descriptor? v)
  (or (initialized-singleton-descriptor? v)
      (uninitialized-singleton-descriptor? v)))

(define (make-singleton-implementation
         type
         #:inspector [inspector (current-inspector)]
         #:property-maker [prop-maker default-singleton-properties])
  (define (make-tuple-props tuple-descriptor)
    (define predicate (tuple-descriptor-predicate tuple-descriptor))
    (prop-maker
     (uninitialized-singleton-descriptor #:type type #:predicate predicate)))
  (define type/tuple
    (tuple-type (singleton-type-name type) 0
                #:predicate-name (singleton-type-predicate-name type)))
  (define descriptor
    (make-tuple-implementation type/tuple
                               #:property-maker make-tuple-props))
  (define instance ((tuple-descriptor-constructor descriptor)))
  (define pred (tuple-descriptor-predicate descriptor))
  (initialized-singleton-descriptor #:type type
                                    #:instance instance
                                    #:predicate pred))

(define (default-singleton-custom-write descriptor)
  (define name (singleton-type-name (singleton-descriptor-type descriptor)))
  (define str (string-append "#<" (symbol->string name) ">"))
  (λ (this out mode)
    (write-string str out)
    (void)))

(define (default-singleton-object-name descriptor)
  (define name (singleton-type-name (singleton-descriptor-type descriptor)))
  (λ (_) name))

(define (default-singleton-properties descriptor)
  (list (cons prop:object-name (default-singleton-object-name descriptor))
        (cons prop:equal+hash (make-singleton-equal+hash))
        (cons prop:custom-write (default-singleton-custom-write descriptor))))

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
