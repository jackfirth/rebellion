#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [field (unconstrained-domain-> field?)]
  [field? (-> field? boolean?)]
  [field-name (-> field? keyword?)]
  [field-value (-> field? any/c)]
  [field<? (-> field? ... boolean?)]))

(require racket/list
         racket/struct
         rebellion/tuple-type
         rebellion/struct-equal-property)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define (make-properties descriptor)
  (define size (tuple-type-size (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define equal+hash (make-equal+hash-property size accessor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) 'field)
     (λ (this)
       (define name (string-append "#:" (keyword->string (field-name this))))
       (list (unquoted-printing-string name) (field-value this)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define field-descriptor
  (tuple-type-make-implementation
   (tuple-type 'field 2 #:constructor-name 'field-constructor)
   #:property-maker make-properties))

(define field-constructor (tuple-descriptor-constructor field-descriptor))
(define field? (tuple-descriptor-predicate field-descriptor))
(define field-name (make-tuple-field-accessor field-descriptor 0 'name))
(define field-value (make-tuple-field-accessor field-descriptor 1 'value))

(define (field-keyword-function kws kw-args)
  (when (> (length kws) 1)
    (raise-arguments-error 'field
                           "multiple keyword arguments"
                           "keywords" kws
                           "values" kw-args))
  (when (< (length kws) 1)
    (raise-arguments-error 'field "no arguments given"))
  (field-constructor (first kws) (first kw-args)))

(define field
  (procedure-reduce-keyword-arity
   (make-keyword-procedure field-keyword-function)
   0
   empty
   #f))

(module+ test
  (test-case "field"
    (define f (field #:widget-price #e49.99))
    (check-equal? (field-name f) '#:widget-price)
    (check-equal? (field-value f) #e49.99)
    (check-equal? f (field #:widget-price #e49.99))
    (check-equal? (~v f) "(field #:widget-price 4999/100)")
    (check-equal? (~a f) "#<field: #:widget-price 4999/100>")
    (check-equal? (~s f) "#<field: #:widget-price 4999/100>")))

(define (field<? . fields)
  (or (empty? fields)
      (apply keyword<? (map field-name fields))))

(module+ test
  (test-case "field<?"
    (check-true (field<?))
    (check-true (field<? (field #:a 1)))
    (check-true (field<? (field #:a 1) (field #:b 1)))
    (check-true (field<? (field #:a 1) (field #:b 0)))
    (check-true (field<? (field #:a 1) (field #:b 2)))
    (check-false (field<? (field #:b 1) (field #:a 1)))
    (check-false (field<? (field #:b 1) (field #:a 0)))
    (check-false (field<? (field #:b 1) (field #:a 2)))
    (check-false (field<? (field #:a 1) (field #:a 1)))
    (check-false (field<? (field #:a 1) (field #:a 0)))
    (check-false (field<? (field #:a 1) (field #:a 2)))
    (check-true (field<? (field #:a 1) (field #:b 1) (field #:c 1)))))
