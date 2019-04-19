#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [initialized-variant-descriptor? (-> any/c boolean?)]
  [make-variant-implementation
   (->* (variant-type?)
        (#:inspector inspector?
         #:property-maker (-> uninitialized-variant-descriptor?
                              (listof (cons/c struct-type-property? any/c))))
        initialized-variant-descriptor?)]
  [uninitialized-variant-descriptor? (-> any/c boolean?)]
  [variant-descriptor? (-> any/c boolean?)]
  [variant-descriptor-accessor (-> variant-descriptor? (-> any/c any/c))]
  [variant-descriptor-constructor (-> variant-descriptor? procedure?)]
  [variant-descriptor-discriminator
   (-> variant-descriptor? (-> any/c natural?))]
  [variant-descriptor-predicate (-> variant-descriptor? (-> any/c boolean?))]
  [variant-descriptor-type (-> variant-descriptor? variant-type?)]
  [variant-type procedure?]
  [variant-type? (-> any/c boolean?)]
  [variant-type-name (-> variant-type? interned-symbol?)]
  [variant-type-size (-> variant-type? natural?)]
  [variant-type-cases (-> variant-type? keyset?)]
  [variant-type-predicate-name (-> variant-type? interned-symbol?)]
  [variant-type-constructor-name (-> variant-type? interned-symbol?)]
  [variant-type-discriminator-name (-> variant-type? interned-symbol?)]
  [variant-type-accessor-name (-> variant-type? interned-symbol?)]))

(require racket/list
         racket/math
         racket/splicing
         racket/struct
         rebellion/generative-token
         rebellion/keyset
         rebellion/record
         rebellion/symbol
         rebellion/tuple-type-definition
         rebellion/tuple-type)

;@------------------------------------------------------------------------------

(define-tuple-type variant-type
  (name cases predicate-name constructor-name discriminator-name accessor-name)
  #:constructor constructor:variant-type)

(define (default-name fmt base-name)
  (string->symbol (format fmt base-name)))

(define (variant-type
         name cases
         #:predicate-name [predicate-name (default-name "~a?" name)]
         #:constructor-name [constructor-name
                             (default-name "constructor:~a" name)]
         #:discriminator-name [discriminator-name
                               (default-name "discriminator:~a" name)]
         #:accessor-name [accessor-name (default-name "accessor:~a" name)])
  (constructor:variant-type name
                            cases
                            predicate-name
                            constructor-name
                            discriminator-name
                            accessor-name))

(define (variant-type-size type) (keyset-size (variant-type-cases type)))

;@------------------------------------------------------------------------------

(define-tuple-type uninitialized-variant-descriptor
  (type predicate constructor discriminator accessor)
  #:constructor constructor:uninitialized-variant-descriptor)

(define-tuple-type initialized-variant-descriptor
  (type predicate constructor discriminator accessor struct-info)
  #:constructor constructor:initialized-variant-descriptor)


(define (make-uninitialized-variant-descriptor #:type type
                                               #:predicate predicate
                                               #:constructor constructor
                                               #:discriminator discriminator
                                               #:accessor accessor)
  (constructor:uninitialized-variant-descriptor
   type predicate constructor discriminator accessor))

(define (make-initialized-variant-descriptor #:type type
                                             #:predicate predicate
                                             #:constructor constructor
                                             #:discriminator discriminator
                                             #:accessor accessor
                                             #:struct-info struct-info)
  (constructor:initialized-variant-descriptor
   type predicate constructor discriminator accessor struct-info))

(define (variant-descriptor? v)
  (or (initialized-variant-descriptor? v)
      (uninitialized-variant-descriptor? v)))

(splicing-local
    [(define ((make-getter initialized uninitialized) descriptor)
       (if (initialized-variant-descriptor? descriptor)
           (initialized descriptor)
           (uninitialized descriptor)))]
  
  (define-values (variant-descriptor-type
                  variant-descriptor-predicate
                  variant-descriptor-constructor
                  variant-descriptor-discriminator
                  variant-descriptor-accessor)
    (values (make-getter initialized-variant-descriptor-type
                         uninitialized-variant-descriptor-type)
            (make-getter initialized-variant-descriptor-predicate
                         uninitialized-variant-descriptor-predicate)
            (make-getter initialized-variant-descriptor-constructor
                         uninitialized-variant-descriptor-constructor)
            (make-getter initialized-variant-descriptor-discriminator
                         uninitialized-variant-descriptor-discriminator)
            (make-getter initialized-variant-descriptor-accessor
                         uninitialized-variant-descriptor-accessor))))

;@------------------------------------------------------------------------------

(define (make-variant-equal+hash descriptor)
  (define token (make-generative-token))
  (define discriminator (variant-descriptor-discriminator descriptor))
  (define accessor (variant-descriptor-discriminator descriptor))
  (define (equal-proc this other recur)
    (and (eq? (discriminator this) (discriminator other))
         (recur (accessor this) (accessor other))))
  (define (hash-proc this recur)
    (recur (list token (discriminator this) (accessor this))))
  (define hash2-proc hash-proc)
  (list equal-proc hash-proc hash2-proc))

(define (make-variant-custom-write descriptor)
  (define type (variant-descriptor-type descriptor))
  (define name (variant-type-name type))
  (define cases (variant-type-cases type))
  (define discriminator (variant-descriptor-discriminator descriptor))
  (define accessor (variant-descriptor-accessor descriptor))
  (define subtype-names
    (build-record (λ (key) (string->symbol (format "~a-~a" key name))) cases))
  (make-constructor-style-printer
   (λ (this) (record-ref subtype-names (discriminator this)))
   (λ (this) (list (accessor this)))))

(define (make-default-variant-properties descriptor)
  (define equal+hash (make-variant-equal+hash descriptor))
  (define custom-write (make-variant-custom-write descriptor))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define (make-variant-style-constructor regular-constructor keys name)
  (procedure-rename
   (make-keyword-procedure
    (λ (kws kw-args)
      (check-only-one-keyword-argument-given name kws kw-args)
      (define kw (first kws))
      (define arg (first kw-args))
      (check-keyword-argument-allowed name kw keys)
      (regular-constructor (keyset-index-of kw) arg)))
   name))

(define (check-only-one-keyword-argument-given who kws kw-args)
  (when (> (length kws) 1)
    (raise-arguments-error who
                           "too many keyword arguments, expected only one"
                           "keywords" kws
                           "values" kw-args))
  (when (< (length kws) 1)
    (raise-arguments-error who "no keyword arguments given, expected one")))

(define (check-keyword-argument-allowed who kw keys)
  (unless (keyset-contains? keys kw)
    (define message "keyword argument does not correspond to a variant case")
    (raise-arguments-error who message
                           "keyword" kw
                           "variant cases" keys)))

(define (make-variant-implementation
         type
         #:inspector [inspector (current-inspector)]
         #:property-maker [prop-maker make-default-variant-properties])
  (define predicate-name (variant-type-predicate-name type))
  (define accessor-name (variant-type-accessor-name type))
  (define constructor-name (variant-type-constructor-name type))
  (define discriminator-name (variant-type-discriminator-name type))
  (define type/tuple
    (tuple-type (variant-type-name type) 2
                #:predicate-name predicate-name
                #:constructor-name constructor-name
                #:accessor-name accessor-name))
  (define (prop-maker/tuple descriptor)
    (define constructor
      (make-variant-style-constructor (tuple-descriptor-constructor descriptor)
                                      (variant-type-cases type)
                                      constructor-name))
    (define accessor/tuple (tuple-descriptor-accessor descriptor))
    (define accessor
      (procedure-rename (λ (this) (accessor/tuple this 1)) accessor-name))
    (define discriminator
      (procedure-rename (λ (this) (accessor/tuple this 0)) discriminator-name))
    (prop-maker
     (make-uninitialized-variant-descriptor
      #:type type
      #:predicate (tuple-descriptor-predicate descriptor)
      #:constructor constructor
      #:accessor accessor
      #:discriminator discriminator)))
  (define descriptor/tuple
    (tuple-type-make-implementation type/tuple
                                    #:inspector inspector
                                    #:property-maker prop-maker/tuple))
  ;; TODO: implement the rest of this
  (make-initialized-variant-descriptor
   #:type type))

;@------------------------------------------------------------------------------
;; TODO: implement the define-variant-type definition macro
