#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-wrapper-implementation
   (->* (wrapper-type?)
        (#:property-maker (-> uninitialized-wrapper-descriptor?
                              (listof (cons/c struct-type-property? any/c)))
         #:inspector inspector?)
        initialized-wrapper-descriptor?)]
  [wrapper-descriptor? predicate/c]
  [uninitialized-wrapper-descriptor? predicate/c]
  [initialized-wrapper-descriptor? predicate/c]
  [wrapper-descriptor-constructor (-> wrapper-descriptor? (-> any/c any/c))]
  [wrapper-descriptor-predicate (-> wrapper-descriptor? predicate/c)]
  [wrapper-descriptor-accessor (-> wrapper-descriptor? (-> any/c any/c))]
  [default-wrapper-properties
   (-> wrapper-descriptor? (listof (cons/c struct-type-property? any/c)))]
  [default-wrapper-equal+hash (-> wrapper-descriptor? equal+hash/c)]
  [default-wrapper-custom-write
   (-> wrapper-descriptor? custom-write-function/c)]))

(require racket/struct
         rebellion/base/generative-token
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/type/record
         rebellion/type/tuple
         rebellion/type/wrapper/base)

;@------------------------------------------------------------------------------

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
         #:property-maker [prop-maker default-wrapper-properties]
         #:inspector [inspector (current-inspector)])
  (define tuple-impl-type
    (tuple-type (wrapper-type-name type) (list 'value)
                #:predicate-name (wrapper-type-predicate-name type)
                #:constructor-name (wrapper-type-constructor-name type)))
  (define (tuple-impl-prop-maker tuple-impl)
    (define tuple-impl-accessor (tuple-descriptor-accessor tuple-impl))
    (define (accessor this) (tuple-impl-accessor this 0))
    (define accessor-name (wrapper-type-accessor-name type))
    (prop-maker
     (uninitialized-wrapper-descriptor
      #:type type
      #:predicate (tuple-descriptor-predicate tuple-impl)
      #:constructor (tuple-descriptor-constructor tuple-impl)
      #:accessor (procedure-rename accessor accessor-name))))
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
   #:accessor (procedure-rename accessor (wrapper-type-accessor-name type))))

(define (make-delegating-equal+hash delegate-extractor)
  (define token (make-generative-token))
  (define (equal-proc this other recur)
    (recur (delegate-extractor this) (delegate-extractor other)))
  (define (hash-proc this recur)
    (recur (cons token (delegate-extractor this))))
  (define hash2-proc hash-proc)
  (list equal-proc hash-proc hash2-proc))

(define (default-wrapper-equal+hash descriptor)
  (make-delegating-equal+hash (wrapper-descriptor-accessor descriptor)))

(define (default-wrapper-custom-write descriptor)
  (define type-name (wrapper-type-name (wrapper-descriptor-type descriptor)))
  (define accessor (wrapper-descriptor-accessor descriptor))
  (make-constructor-style-printer
     (λ (_) type-name)
     (λ (this) (list (accessor this)))))

(define (default-wrapper-properties descriptor)
  (list (cons prop:equal+hash (default-wrapper-equal+hash descriptor))
        (cons prop:custom-write (default-wrapper-custom-write descriptor))
        (cons prop:custom-print-quotable 'never)))
