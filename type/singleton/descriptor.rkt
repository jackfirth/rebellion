#lang racket/base

(require racket/contract/base
         racket/contract/combinator)

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
  [singleton-descriptor-type (-> singleton-descriptor? singleton-type?)]
  [singleton-descriptor-instance (-> initialized-singleton-descriptor? any/c)]
  [singleton-descriptor-predicate (-> singleton-descriptor? predicate/c)]
  [default-singleton-properties
   (-> singleton-descriptor? (listof (cons/c struct-type-property? any/c)))]
  [default-singleton-custom-write
   (-> singleton-descriptor? custom-write-function/c)]
  [default-singleton-object-name (-> singleton-descriptor? object-name/c)]
  [default-singleton-flat-contract (-> singleton-descriptor? flat-contract-property?)]))

(require racket/math
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/type/singleton/base
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define (write-descriptor this out _)
  (define name (object-name this))
  (write-string "#<singleton-descriptor:" out)
  (write-string (symbol->string name) out)
  (write-string ">" out)
  (void))

(struct initialized-singleton-descriptor
  (type predicate instance backing-tuple-descriptor)
  #:omit-define-syntaxes
  #:constructor-name constructor:initialized-singleton-descriptor

  #:property prop:object-name
  (λ (this) (singleton-type-name (initialized-singleton-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(struct uninitialized-singleton-descriptor
  (type predicate)
  #:omit-define-syntaxes
  #:constructor-name constructor:uninitialized-singleton-descriptor

  #:property prop:object-name
  (λ (this) (singleton-type-name (uninitialized-singleton-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(define (initialized-singleton-descriptor
         #:type type
         #:predicate predicate
         #:instance instance
         #:backing-tuple-descriptor tuple-descriptor)
  (constructor:initialized-singleton-descriptor
   type predicate instance tuple-descriptor))

(define (uninitialized-singleton-descriptor
         #:type type
         #:predicate predicate)
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
    (tuple-type (singleton-type-name type) (list)
                #:predicate-name (singleton-type-predicate-name type)))
  (define descriptor
    (make-tuple-implementation type/tuple
                               #:property-maker make-tuple-props))
  (define instance ((tuple-descriptor-constructor descriptor)))
  (define pred (tuple-descriptor-predicate descriptor))
  (initialized-singleton-descriptor
   #:type type
   #:instance instance
   #:predicate pred
   #:backing-tuple-descriptor descriptor))

(define (default-singleton-custom-write descriptor)
  (define name (singleton-type-name (singleton-descriptor-type descriptor)))
  (define str (string-append "#<" (symbol->string name) ">"))
  (λ (this out mode)
    (write-string str out)
    (void)))

(define (default-singleton-object-name descriptor)
  (define name (singleton-type-name (singleton-descriptor-type descriptor)))
  (λ (_) name))

(define (default-singleton-flat-contract descriptor)
  (build-flat-contract-property
   #:name (default-singleton-object-name descriptor)
   #:first-order (λ (this) (λ (x) (eq? x this)))))

(define (default-singleton-properties descriptor)
  (list (cons prop:object-name (default-singleton-object-name descriptor))
        (cons prop:equal+hash (make-singleton-equal+hash))
        (cons prop:custom-write (default-singleton-custom-write descriptor))
        (cons prop:custom-print-quotable 'never)
        (cons prop:flat-contract (default-singleton-flat-contract descriptor))))

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

(define object-name/c (or/c natural? (-> any/c any/c)))
