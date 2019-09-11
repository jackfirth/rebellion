#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-record-implementation
   (->* (record-type?)
        (#:inspector inspector?
         #:property-maker (-> uninitialized-record-descriptor? properties/c))
        initialized-record-descriptor?)]
  [record-descriptor? predicate/c]
  [initialized-record-descriptor? predicate/c]
  [uninitialized-record-descriptor? predicate/c]
  [record-descriptor-type (-> record-descriptor? record-type?)]
  [record-descriptor-predicate (-> record-descriptor? predicate/c)]
  [record-descriptor-constructor (-> record-descriptor? procedure?)]
  [record-descriptor-accessor (-> record-descriptor? procedure?)]
  [make-default-record-properties (-> record-descriptor? properties/c)]
  [make-record-equal+hash (-> record-descriptor? equal+hash/c)]
  [make-record-custom-write (-> record-descriptor? custom-write-function/c)]
  [make-record-field-accessor (-> record-descriptor? natural? procedure?)]))

(require racket/math
         racket/struct
         rebellion/collection/keyset/low-dependency
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/private/spliced-printing-entry
         rebellion/type/record/base
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define properties/c (listof (cons/c struct-type-property? any/c)))

(define-tuple-type initialized-record-descriptor
  (type predicate constructor accessor))

(define-tuple-type uninitialized-record-descriptor
  (type predicate constructor accessor))

(define (record-descriptor? v)
  (or (initialized-record-descriptor? v) (uninitialized-record-descriptor? v)))

(define ((make-getter initialized uninitialized) descriptor)
  (if (initialized-record-descriptor? descriptor)
      (initialized descriptor)
      (uninitialized descriptor)))

(define record-descriptor-type
  (make-getter initialized-record-descriptor-type
               uninitialized-record-descriptor-type))

(define record-descriptor-predicate
  (make-getter initialized-record-descriptor-predicate
               uninitialized-record-descriptor-predicate))

(define record-descriptor-constructor
  (make-getter initialized-record-descriptor-constructor
               uninitialized-record-descriptor-constructor))

(define record-descriptor-accessor
  (make-getter initialized-record-descriptor-accessor
               uninitialized-record-descriptor-accessor))

(define (make-record-implementation
         type
         #:inspector [inspector (current-inspector)]
         #:property-maker [prop-maker make-default-record-properties])
  (define (tuple-prop-maker descriptor)
    (prop-maker (tuple-descriptor->record-descriptor descriptor type)))
  (define descriptor
    (make-tuple-implementation (record-type->tuple-type type)
                                    #:inspector inspector
                                    #:property-maker tuple-prop-maker))
  (tuple-descriptor->record-descriptor descriptor type))

(define (record-type->tuple-type type)
  (tuple-type (record-type-name type)
              (keyset-size (record-type-fields type))
              #:predicate-name (record-type-predicate-name type)
              #:constructor-name (record-type-constructor-name type)
              #:accessor-name (record-type-accessor-name type)))

(define (tuple-descriptor->record-descriptor descriptor type)
  (define type-name (record-type-name type))
  (define maker
    (if (initialized-tuple-descriptor? descriptor)
        initialized-record-descriptor
        uninitialized-record-descriptor))
  (define predicate (tuple-descriptor-predicate descriptor))
  (define tuple-constructor (tuple-descriptor-constructor descriptor))
  (define fields (keyset->list (record-type-fields type)))
  (define raw-constructor
    (make-keyword-procedure (λ (unused-kws vs) (apply tuple-constructor vs))))
  (define constructor
    (procedure-rename
     (procedure-reduce-keyword-arity raw-constructor 0 fields fields)
     (object-name tuple-constructor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (maker type predicate constructor accessor))

(define (make-default-record-properties descriptor)
  (define equal+hash (make-record-equal+hash descriptor))
  (define custom-write (make-record-custom-write descriptor))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define (make-record-equal+hash descriptor)
  (define accessor (record-descriptor-accessor descriptor))
  (define size
    (keyset-size (record-type-fields (record-descriptor-type descriptor))))
  (make-accessor-based-equal+hash accessor size))

(define (unquoted-printing-keyword kw)
  (unquoted-printing-string (string-append "#:" (keyword->string kw))))

(define (make-record-custom-write descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define accessor (record-descriptor-accessor descriptor))
  (define fields (record-type-fields type))
  (define size (keyset-size fields))
  (make-constructor-style-printer
   (λ (_) type-name)
   (λ (this)
     (for*/list ([i (in-range size)]
                 [kw (in-value (keyset-ref fields i))])
       (define v (accessor this i))
       (define kw-str (unquoted-printing-keyword kw))
       (spliced-printing-entry kw-str v)))))

(define (make-record-field-accessor descriptor field)
  (define accessor (record-descriptor-accessor descriptor))
  (define type (record-descriptor-type descriptor))
  (define fields (record-type-fields type))
  (define name
    (string->symbol
     (string-append (symbol->string (record-type-name type))
                    "-"
                    (keyword->string (keyset-ref fields field)))))
  (procedure-rename (λ (this) (accessor this field)) name))
