#lang racket/base

(require racket/contract/base)

(provide
 define-wrapper-type
 (contract-out
  [wrapper-type? predicate/c]
  [wrapper-type procedure?]
  [make-wrapper-implementation
   (->* (wrapper-type?)
        (#:property-maker (-> uninitialized-wrapper-descriptor?
                              (listof (cons/c struct-type-property? any/c)))
         #:inspector inspector?)
        initialized-wrapper-descriptor?)]
  [wrapper-type-name (-> wrapper-type? interned-symbol?)]
  [wrapper-type-constructor-name (-> wrapper-type? interned-symbol?)]
  [wrapper-type-predicate-name (-> wrapper-type? interned-symbol?)]
  [wrapper-type-accessor-name (-> wrapper-type? interned-symbol?)]
  [wrapper-descriptor? predicate/c]
  [uninitialized-wrapper-descriptor? predicate/c]
  [initialized-wrapper-descriptor? predicate/c]
  [wrapper-descriptor-constructor (-> wrapper-descriptor? (-> any/c any/c))]
  [wrapper-descriptor-predicate (-> wrapper-descriptor? predicate/c)]
  [wrapper-descriptor-accessor (-> wrapper-descriptor? (-> any/c any/c))]
  [make-default-wrapper-properties
   (-> uninitialized-wrapper-descriptor?
       (listof (cons/c struct-type-property? any/c)))]))

(require racket/struct
         rebellion/generative-token
         rebellion/symbol
         rebellion/type/record
         rebellion/type/tuple
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define (format-symbol template . vs)
  (string->symbol (apply format template vs)))

;@------------------------------------------------------------------------------

(define-record-type wrapper-type
  (name predicate-name constructor-name accessor-name)
  #:constructor-name constructor:wrapper-type)

(define (wrapper-type
         name
         #:predicate-name [predicate-name (format-symbol "~a?" name)]
         #:constructor-name [constructor-name name]
         #:accessor-name [accessor-name (format-symbol "~a-value" name)])
  (constructor:wrapper-type
   #:name name
   #:predicate-name predicate-name
   #:constructor-name constructor-name
   #:accessor-name accessor-name))

(define-record-type initialized-wrapper-descriptor
  (type predicate constructor accessor))

(define-record-type uninitialized-wrapper-descriptor
  (type predicate constructor accessor))

(define (wrapper-descriptor? v)
  (or (uninitialized-wrapper-descriptor? v)
      (initialized-wrapper-descriptor? v)))

(define (wrapper-descriptor-type desc)
  (if (uninitialized-wrapper-descriptor? desc)
      (uninitialized-wrapper-descriptor-type desc)
      (initialized-wrapper-descriptor-type desc)))

(define (wrapper-descriptor-predicate desc)
  (if (uninitialized-wrapper-descriptor? desc)
      (uninitialized-wrapper-descriptor-predicate desc)
      (initialized-wrapper-descriptor-predicate desc)))

(define (wrapper-descriptor-constructor desc)
  (if (uninitialized-wrapper-descriptor? desc)
      (uninitialized-wrapper-descriptor-constructor desc)
      (initialized-wrapper-descriptor-constructor desc)))

(define (wrapper-descriptor-accessor desc)
  (if (uninitialized-wrapper-descriptor? desc)
      (uninitialized-wrapper-descriptor-accessor desc)
      (initialized-wrapper-descriptor-accessor desc)))

(define (make-wrapper-implementation
         type
         #:property-maker [prop-maker make-default-wrapper-properties]
         #:inspector [inspector (current-inspector)])
  (define tuple-impl-type
    (tuple-type (wrapper-type-name type) 1
                #:predicate-name (wrapper-type-predicate-name type)
                #:constructor-name (wrapper-type-constructor-name type)))
  (define (tuple-impl-prop-maker tuple-impl)
    (prop-maker
      (uninitialized-wrapper-descriptor
       #:type type
       #:predicate (tuple-descriptor-predicate tuple-impl)
       #:constructor (tuple-descriptor-constructor tuple-impl)
       #:accessor (procedure-rename accessor (tuple-type-accessor-name type)))))
  (define tuple-impl
    (make-tuple-implementation tuple-impl-type
                               #:inspector inspector
                               #:property-maker tuple-impl-prop-maker))
  (define tuple-impl-accessor (tuple-descriptor-accessor tuple-impl))
  (define (accessor this) (tuple-impl-accessor this 0))
  (initialized-wrapper-descriptor
   #:type type
   #:predicate (tuple-descriptor-predicate tuple-impl)
   #:constructor (tuple-descriptor-constructor tuple-impl)
   #:accessor (procedure-rename accessor (tuple-type-accessor-name type))))

(define (make-delegating-equal+hash delegate-extractor)
  (define token (make-generative-token))
  (define (equal-proc this other recur)
    (recur (delegate-extractor this) (delegate-extractor other)))
  (define (hash-proc this recur)
    (recur (cons token (delegate-extractor this))))
  (define hash2-proc hash-proc)
  (list equal-proc hash-proc hash2-proc))

;; TODO: finish this function
(define (make-default-wrapper-properties descriptor)
  (define type (wrapper-descriptor-type descriptor))
  (define type-name (wrapper-type-name type))
  (define accessor (wrapper-descriptor-accessor descriptor))
  (define equal+hash (make-delegating-equal+hash accessor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) type-name)
     (λ (this) (list (accessor this)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-simple-macro (define-wrapper-type id:id) (begin))
