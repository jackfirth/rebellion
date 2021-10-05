#lang racket/base

(require racket/contract/base racket/contract/region)

(provide
 wrapper-guard-maker/c
 (contract-out
  [make-wrapper-implementation
   (->* (wrapper-type?)
        (#:property-maker (-> uninitialized-wrapper-descriptor?
                              (listof (cons/c struct-type-property? any/c)))
         #:guard-maker (or/c #f
                             (-> uninitialized-wrapper-descriptor?
                                 (-> any/c any/c)))
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
         syntax/location
         rebellion/base/generative-token
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/type/tuple
         rebellion/type/wrapper/base
         (for-syntax racket/base syntax/parse))

;@------------------------------------------------------------------------------

(define (write-descriptor this out _)
  (define name (object-name this))
  (write-string "#<wrapper-descriptor:" out)
  (write-string (symbol->string name) out)
  (write-string ">" out)
  (void))

(struct initialized-wrapper-descriptor
  (type predicate constructor accessor backing-tuple-descriptor)
  #:omit-define-syntaxes
  #:constructor-name constructor:initialized-wrapper-descriptor

  #:property prop:object-name
  (λ (this) (wrapper-type-name (initialized-wrapper-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(struct uninitialized-wrapper-descriptor
  (type predicate constructor accessor)
  #:omit-define-syntaxes
  #:constructor-name constructor:uninitialized-wrapper-descriptor

  #:property prop:object-name
  (λ (this) (wrapper-type-name (uninitialized-wrapper-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(define (initialized-wrapper-descriptor
         #:type type
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor
         #:backing-tuple-descriptor tuple-descriptor)
  (constructor:initialized-wrapper-descriptor
   type predicate constructor accessor tuple-descriptor))

(define (uninitialized-wrapper-descriptor
         #:type type
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor)
  (constructor:uninitialized-wrapper-descriptor
   type predicate constructor accessor))

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
         #:guard-maker [guard-maker #f]
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
  (define accessor
    (procedure-rename
     (λ (this) (tuple-impl-accessor this 0))
     (wrapper-type-accessor-name type)))
  (define predicate (tuple-descriptor-predicate tuple-impl))
  (define tuple-constructor (tuple-descriptor-constructor tuple-impl))
  (define constructor
    (if guard-maker
        (let ([guard
               (guard-maker
                (uninitialized-wrapper-descriptor
                 #:type type
                 #:predicate predicate
                 #:constructor tuple-constructor
                 #:accessor accessor))])
          (λ (value) (tuple-constructor (guard value))))
        tuple-constructor))
  (initialized-wrapper-descriptor
   #:type type
   #:predicate predicate 
   #:constructor constructor
   #:accessor accessor
   #:backing-tuple-descriptor tuple-impl))

(define-syntax (wrapper-guard-maker/c stx)
  (syntax-parse stx
    [(_ contract-expr:expr)
     (quasisyntax/loc stx
       (let ([loc (quote-srcloc #,stx)]
             ;; There's no way to blame the caller,
             ;; so we have to take the blame for ourselves.
             ;; (this is also what struct-guard/c does)
             [blame-party (current-contract-region)])
         (λ (desc)
           (define name (wrapper-type-constructor-name (wrapper-descriptor-type desc)))
           (λ (value)
             (contract contract-expr value
                       blame-party blame-party
                       name loc)))))]))

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
