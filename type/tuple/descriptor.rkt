#lang racket/base

(require racket/contract/base racket/contract/region)

(provide
 tuple-guard-maker/c
 (contract-out
  [initialized-tuple-descriptor? (-> any/c boolean?)]
  [default-tuple-properties
   (-> uninitialized-tuple-descriptor?
       (listof (cons/c struct-type-property? any/c)))]
  [default-tuple-equal+hash (-> tuple-descriptor? equal+hash/c)]
  [default-tuple-custom-write (-> tuple-descriptor? custom-write-function/c)]
  [make-tuple-field-accessor
   (->i ([desc () tuple-descriptor?]
         [pos (desc)
              (and/c natural?
                     (</c (tuple-type-size (tuple-descriptor-type desc))))])
        [_ (desc) (-> (tuple-descriptor-predicate desc) any/c)])]
  [tuple-descriptor? (-> any/c boolean?)]
  [tuple-descriptor-accessor (-> tuple-descriptor? (-> any/c natural? any/c))]
  [tuple-descriptor-constructor (-> tuple-descriptor? procedure?)]
  [tuple-descriptor-predicate (-> tuple-descriptor? (-> any/c boolean?))]
  [tuple-descriptor-type (-> tuple-descriptor? tuple-type?)]
  [make-tuple-implementation
   (->* (tuple-type?)
        (#:guard (or/c procedure? #f)
         #:inspector inspector?
         #:property-maker
         (-> uninitialized-tuple-descriptor?
             (listof (cons/c struct-type-property? any/c)))
         #:guard-maker
         (or/c #f (-> uninitialized-tuple-descriptor? procedure?)))
        initialized-tuple-descriptor?)]
  [uninitialized-tuple-descriptor? (-> any/c boolean?)]
  [tuple-impersonate
   (->i #:chaperone
        ([instance (descriptor) (tuple-descriptor-predicate descriptor)]
         [descriptor initialized-tuple-descriptor?])
        (#:properties [properties impersonator-property-hash/c]
         #:chaperone? [chaperone? boolean?])
        [_ (descriptor) (tuple-descriptor-predicate descriptor)])]))

(require racket/math
         racket/struct
         syntax/location
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/private/impersonation
         rebellion/type/tuple/base
         rebellion/type/struct
         (for-syntax racket/syntax racket/base syntax/parse))

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define (default-tuple-equal+hash descriptor)
  (define accessor (tuple-descriptor-accessor descriptor))
  (define size (tuple-type-size (tuple-descriptor-type descriptor)))
  (make-accessor-based-equal+hash accessor size))

(define (default-tuple-custom-write descriptor)
  (define type (tuple-descriptor-type descriptor))
  (define type-name (tuple-type-name type))
  (define size (tuple-type-size type))
  (define accessor (tuple-descriptor-accessor descriptor))
  (make-constructor-style-printer
   (λ (_) type-name)
   (λ (this) (build-list size (λ (pos) (accessor this pos))))))

(define (default-tuple-properties descriptor)
  (list (cons prop:equal+hash (default-tuple-equal+hash descriptor))
        (cons prop:custom-write (default-tuple-custom-write descriptor))
        (cons prop:custom-print-quotable 'never)))

;@------------------------------------------------------------------------------

(define (write-descriptor this out _)
  (define name (object-name this))
  (write-string "#<tuple-descriptor:" out)
  (write-string (symbol->string name) out)
  (write-string ">" out)
  (void))

(struct initialized-tuple-descriptor
  (type predicate constructor accessor backing-struct-type)
  #:omit-define-syntaxes
  #:constructor-name constructor:initialized-tuple-descriptor

  #:property prop:object-name
  (λ (this) (tuple-type-name (initialized-tuple-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(struct uninitialized-tuple-descriptor
  (type predicate constructor accessor)
  #:omit-define-syntaxes
  #:constructor-name constructor:uninitialized-tuple-descriptor

  #:property prop:object-name
  (λ (this) (tuple-type-name (uninitialized-tuple-descriptor-type this)))

  #:property prop:custom-write write-descriptor
  #:property prop:custom-print-quotable 'never)

(define (initialized-tuple-descriptor
         #:type type
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor
         #:backing-struct-type struct-type)
  (constructor:initialized-tuple-descriptor
   type predicate constructor accessor struct-type))

(define (uninitialized-tuple-descriptor
         #:type type
         #:predicate predicate
         #:constructor constructor
         #:accessor accessor)
  (constructor:uninitialized-tuple-descriptor
   type predicate constructor accessor))

;@------------------------------------------------------------------------------

(define (tuple-descriptor? v)
  (or (initialized-tuple-descriptor? v)
      (uninitialized-tuple-descriptor? v)))

(define (tuple-descriptor-case descriptor #:initialized f #:uninitialized g)
  (if (initialized-tuple-descriptor? descriptor)
      (f descriptor)
      (g descriptor)))

(define (tuple-descriptor-type descriptor)
  (tuple-descriptor-case descriptor
                         #:initialized initialized-tuple-descriptor-type
                         #:uninitialized uninitialized-tuple-descriptor-type))

(define (tuple-descriptor-predicate descriptor)
  (tuple-descriptor-case
   descriptor
   #:initialized initialized-tuple-descriptor-predicate
   #:uninitialized uninitialized-tuple-descriptor-predicate))

(define (tuple-descriptor-constructor descriptor)
  (tuple-descriptor-case
   descriptor
   #:initialized initialized-tuple-descriptor-constructor
   #:uninitialized uninitialized-tuple-descriptor-constructor))

(define (tuple-descriptor-accessor descriptor)
  (tuple-descriptor-case
   descriptor
   #:initialized initialized-tuple-descriptor-accessor
   #:uninitialized uninitialized-tuple-descriptor-accessor))

;@------------------------------------------------------------------------------

(define (make-tuple-implementation
         type
         #:guard [guard #f]
         #:inspector [inspector (current-inspector)]
         #:property-maker [prop-maker default-tuple-properties]
         #:guard-maker [guard-maker #f])
  (define (get-predicate descriptor)
    (procedure-rename (struct-descriptor-predicate descriptor)
                      (tuple-type-predicate-name type)))
  (define (get-constructor descriptor)
    (procedure-rename (struct-descriptor-constructor descriptor)
                      (tuple-type-constructor-name type)))
  (define (get-accessor descriptor)
    (procedure-rename (struct-descriptor-accessor descriptor)
                      (tuple-type-accessor-name type)))
  (define (make-uninit-descriptor descriptor)
    (uninitialized-tuple-descriptor
      #:type type
      #:predicate (get-predicate descriptor)
      #:constructor (get-constructor descriptor)
      #:accessor (get-accessor descriptor)))
  (define (struct-prop-maker descriptor)
    (prop-maker (make-uninit-descriptor descriptor)))
  (define (make-constructor descriptor)
    (define raw-constructor (get-constructor descriptor))
    (if guard-maker
        ;; ensure that arity errors are reported on the constructor rather than the guard
        (procedure-reduce-arity
         (compose raw-constructor (guard-maker (make-uninit-descriptor descriptor)))
         (tuple-type-size type)
         (object-name raw-constructor))
        raw-constructor))
  (define descriptor
    (make-struct-implementation
     #:name (tuple-type-name type)
     #:immutable-fields (tuple-type-size type)
     #:constructor-name (tuple-type-constructor-name type)
     #:guard guard
     #:inspector inspector
     #:property-maker struct-prop-maker))
  (initialized-tuple-descriptor
   #:type type
   #:backing-struct-type (struct-descriptor-type descriptor)
   #:predicate (get-predicate descriptor)
   #:constructor (make-constructor descriptor)
   #:accessor (get-accessor descriptor)))

(define-syntax (tuple-guard-maker/c stx)
  (syntax-parse stx
    #:track-literals
    [(_ contract-expr:expr ...)
     (define/with-syntax ([ctc arg-sym arg-name-sym field-index] ...)
       (for/list ([ctc (syntax-e #'(contract-expr ...))]
                  [index (in-naturals 1)])
         (list ctc (gensym) (gensym) index)))
     (define expected-size (length (syntax-e #'(contract-expr ...))))
     (quasisyntax/loc stx
       (let ([loc (quote-srcloc #,stx)]
             [blame-party (current-contract-region)])
         (λ (desc)
           (define tuple-type (tuple-descriptor-type desc))
           (define constructor-name (tuple-type-constructor-name tuple-type))
           #,(if (= 1 expected-size)
                 #'(begin)
                 #'(define-values (arg-name-sym ...)
                     (values (format "~a, field ~a" constructor-name field-index) ...)))
           (define size (tuple-type-size tuple-type))
           (unless (= size #,expected-size)
             (raise-arguments-error
              'tuple-guard-maker/c
              "tuple size mismatch;\n the expected tuple size does not match the given tuple descriptor"
              "expected" #,expected-size
              "given" size))
           (λ (arg-sym ...)
             #,(if (= 1 expected-size)
                   #'(contract ctc ... arg-sym ...
                               blame-party blame-party
                               constructor-name loc)
                   #'(values
                      (contract ctc arg-sym
                                blame-party blame-party
                                arg-name-sym loc)
                      ...))))))]))

(define (make-tuple-field-accessor descriptor pos)
  (define type (tuple-descriptor-type descriptor))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define name (tuple-type-name type))
  (define field-name (vector-ref (tuple-type-fields type) pos))
  (define field-accessor-name (string->symbol (format "~a-~a" name field-name)))
  (procedure-rename (λ (this) (accessor this pos)) field-accessor-name))

(define (tuple-impersonate instance descriptor
                           #:properties [props (hash)]
                           #:chaperone? [chaperone? #t])
  (define struct-type
    (initialized-tuple-descriptor-backing-struct-type descriptor))
  (struct-impersonate instance struct-type
                      #:properties props
                      #:chaperone? chaperone?))

(module+ test
  (test-case "make-tuple-implementation"
    (define point-type (tuple-type 'point (list 'x 'y)))
    (define point-descriptor (make-tuple-implementation point-type))
    (define point (tuple-descriptor-constructor point-descriptor))
    (define point? (tuple-descriptor-predicate point-descriptor))
    (define point-x (make-tuple-field-accessor point-descriptor 0))
    (define point-y (make-tuple-field-accessor point-descriptor 1))
    (define p (point 42 1000))
    (check-pred point? p)
    (check-equal? p (point 42 1000))
    (check-equal? (point-x p) 42)
    (check-equal? (point-y p) 1000)
    (check-equal? (~v p) "(point 42 1000)")
    (check-equal? (~a p) "#<point: 42 1000>")
    (check-equal? (~s p) "#<point: 42 1000>")
    (check-equal? (~v point) "#<procedure:constructor:point>")
    (check-equal? (~v point?) "#<procedure:point?>")
    (check-equal? (~v point-x) "#<procedure:point-x>")
    (check-equal? (~v point-y) "#<procedure:point-y>")))
