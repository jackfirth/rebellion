#lang racket/base

(require racket/contract/base)

(provide
 define-tuple-type
 (contract-out
  [initialized-tuple-descriptor? (-> any/c boolean?)]
  [make-default-tuple-properties
   (-> uninitialized-tuple-descriptor?
       (listof (cons/c struct-type-property? any/c)))]
  [make-tuple-field-accessor
   (->i ([desc () tuple-descriptor?]
         [pos (desc)
              (and/c natural?
                     (</c (tuple-type-size (tuple-descriptor-type desc))))])
        ([field-name symbol?])
        [_ (desc) (-> (tuple-descriptor-predicate desc) any/c)])]
  [tuple-descriptor? (-> any/c boolean?)]
  [tuple-descriptor-accessor (-> tuple-descriptor? (-> any/c natural? any/c))]
  [tuple-descriptor-constructor (-> tuple-descriptor? procedure?)]
  [tuple-descriptor-predicate (-> tuple-descriptor? (-> any/c boolean?))]
  [tuple-descriptor-struct-type (-> initialized-tuple-descriptor? struct-type?)]
  [tuple-descriptor-type (-> tuple-descriptor? tuple-type?)]
  [tuple-type
   (->* (symbol? natural?)
        (#:predicate-name (or/c symbol? #f)
         #:constructor-name (or/c symbol? #f)
         #:accessor-name (or/c symbol? #f))
        tuple-type?)]
  [tuple-type? (-> any/c boolean?)]
  [tuple-type-accessor-name (-> tuple-type? symbol?)]
  [tuple-type-constructor-name (-> tuple-type? symbol?)]
  [make-tuple-implementation
   (->* (tuple-type?)
        (#:guard (or/c procedure? #f)
         #:inspector inspector?
         #:property-maker
         (-> uninitialized-tuple-descriptor?
             (listof (cons/c struct-type-property? any/c))))
        initialized-tuple-descriptor?)]
  [tuple-type-name (-> tuple-type? symbol?)]
  [tuple-type-predicate-name (-> tuple-type? symbol?)]
  [tuple-type-size (-> tuple-type? natural?)]
  [uninitialized-tuple-descriptor? (-> any/c boolean?)]))

(require (for-syntax racket/base
                     racket/syntax)
         racket/math
         racket/struct
         rebellion/custom-write/struct
         rebellion/equal+hash
         rebellion/equal+hash/struct
         rebellion/collection/keyset
         rebellion/permutation
         rebellion/private/struct-definition-util
         rebellion/struct-descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define (make-transparent-style-properties descriptor)
  (define equal+hash (make-struct-equal+hash descriptor))
  (define custom-write (make-struct-constructor-style-custom-write descriptor))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define (make-descriptor-style-properties descriptor)
  (define accessor (struct-descriptor-accessor descriptor))
  (define type-name (struct-descriptor-name descriptor))
  (define prefix (string-append "#<" (symbol->string type-name) ":"))
  (define equal+hash (make-struct-equal+hash descriptor))
  (define (get-name this) (symbol->string (tuple-type-name (accessor this 0))))
  (define (custom-write this out _)
    (write-string prefix out)
    (write-string (get-name this) out)
    (write-string ">" out))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define (make-default-tuple-properties descriptor)
  (define type (tuple-descriptor-type descriptor))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define name (tuple-type-name type))
  (define size (tuple-type-size type))
  (define equal+hash (make-accessor-based-equal+hash accessor size))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this) (build-list size (λ (pos) (accessor this pos))))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

;@------------------------------------------------------------------------------

(define fields:tuple-type
  (keyset #:name
          #:size
          #:predicate-name
          #:constructor-name
          #:accessor-name))

(define fields:uninitialized-tuple-descriptor
  (keyset #:type #:predicate #:constructor #:accessor))

(define fields:initialized-tuple-descriptor
  (keyset #:type #:predicate #:constructor #:accessor #:struct-type))

(define descriptor:tuple-type
  (make-struct-type/descriptor
   #:name 'tuple-type
   #:immutable-fields (keyset-size fields:tuple-type)
   #:constructor-name 'constructor:tuple-type
   #:property-maker make-transparent-style-properties))

(define descriptor:uninitialized-tuple-descriptor
  (make-struct-type/descriptor
   #:name 'uninitialized-tuple-descriptor
   #:immutable-fields (keyset-size fields:uninitialized-tuple-descriptor)
   #:constructor-name 'constructor:uninitialized-tuple-descriptor
   #:property-maker make-descriptor-style-properties))

(define descriptor:initialized-tuple-descriptor
  (make-struct-type/descriptor
   #:name 'initialized-tuple-descriptor
   #:immutable-fields (keyset-size fields:initialized-tuple-descriptor)
   #:constructor-name 'constructor:initialized-tuple-descriptor
   #:property-maker make-descriptor-style-properties))

(define tuple-type? (struct-descriptor-predicate descriptor:tuple-type))

(define constructor:tuple-type
  (struct-descriptor-constructor descriptor:tuple-type))

(define (tuple-type
         name
         size
         #:predicate-name [predicate-name* #f]
         #:constructor-name [constructor-name* #f]
         #:accessor-name [accessor-name* #f])
  (define predicate-name
    (or predicate-name* (default-tuple-predicate-name name)))
  (define constructor-name (or constructor-name* name))
  (define accessor-name (or accessor-name* (default-tuple-accessor-name name)))
  (constructor:tuple-type
   name size predicate-name constructor-name accessor-name))

(define-struct-field-accessors tuple-type
  (name size predicate-name constructor-name accessor-name)
  #:descriptor descriptor:tuple-type)

(define uninitialized-tuple-descriptor?
  (struct-descriptor-predicate descriptor:uninitialized-tuple-descriptor))

(define constructor:uninitialized-tuple-descriptor
  (struct-descriptor-constructor descriptor:uninitialized-tuple-descriptor))

(define (uninitialized-tuple-descriptor #:type type
                                        #:predicate predicate
                                        #:constructor constructor
                                        #:accessor accessor)
  (constructor:uninitialized-tuple-descriptor
   type predicate constructor accessor))

(define-struct-field-accessors uninitialized-tuple-descriptor
  (type predicate constructor accessor)
  #:descriptor descriptor:uninitialized-tuple-descriptor)

(define initialized-tuple-descriptor?
  (struct-descriptor-predicate descriptor:initialized-tuple-descriptor))

(define constructor:initialized-tuple-descriptor
  (struct-descriptor-constructor descriptor:initialized-tuple-descriptor))

(define (initialized-tuple-descriptor #:type type
                                      #:struct-type struct-type
                                      #:predicate predicate
                                      #:constructor constructor
                                      #:accessor accessor)
  (constructor:initialized-tuple-descriptor
   type predicate constructor accessor struct-type))

(define-struct-field-accessors initialized-tuple-descriptor
  (type predicate constructor accessor struct-type)
  #:descriptor descriptor:initialized-tuple-descriptor)

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

(define (tuple-descriptor-struct-type descriptor)
  (uninitialized-tuple-descriptor-type descriptor))

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
         #:property-maker [prop-maker make-default-tuple-properties])
  (define (get-predicate descriptor)
    (procedure-rename (struct-descriptor-predicate descriptor)
                      (tuple-type-predicate-name type)))
  (define (get-constructor descriptor)
    (procedure-rename (struct-descriptor-constructor descriptor)
                      (tuple-type-constructor-name type)))
  (define (get-accessor descriptor)
    (procedure-rename (struct-descriptor-accessor descriptor)
                      (tuple-type-accessor-name type)))
  (define (struct-prop-maker descriptor)
    (prop-maker
     (uninitialized-tuple-descriptor
      #:type type
      #:predicate (get-predicate descriptor)
      #:constructor (get-constructor descriptor)
      #:accessor (get-accessor descriptor))))
  (define descriptor
    (make-struct-type/descriptor
     #:name (tuple-type-name type)
     #:immutable-fields (tuple-type-size type)
     #:constructor-name (tuple-type-constructor-name type)
     #:guard guard
     #:inspector inspector
     #:property-maker struct-prop-maker))
  (initialized-tuple-descriptor
   #:type type
   #:struct-type (struct-descriptor-type descriptor)
   #:predicate (get-predicate descriptor)
   #:constructor (get-constructor descriptor)
   #:accessor (get-accessor descriptor)))

(define (default-tuple-predicate-name name)
  (string->symbol (format "~a?" name)))

(define (default-tuple-accessor-name name)
  (string->symbol (format "~a-ref" name)))

(define (make-tuple-field-accessor descriptor pos [field-name* #f])
  (define type (tuple-descriptor-type descriptor))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define name (tuple-type-name type))
  (define field-name (or field-name* (string->symbol (format "field~a" pos))))
  (define field-accessor-name (string->symbol (format "~a-~a" name field-name)))
  (procedure-rename (λ (this) (accessor this pos)) field-accessor-name))

(module+ test
  (test-case "make-tuple-implementation"
    (define point-type (tuple-type 'point 2))
    (define point-descriptor (make-tuple-implementation point-type))
    (define point (tuple-descriptor-constructor point-descriptor))
    (define point? (tuple-descriptor-predicate point-descriptor))
    (define point-x (make-tuple-field-accessor point-descriptor 0 'x))
    (define point-y (make-tuple-field-accessor point-descriptor 1 'y))
    (define p (point 42 1000))
    (check-pred point? p)
    (check-equal? p (point 42 1000))
    (check-equal? (point-x p) 42)
    (check-equal? (point-y p) 1000)
    (check-equal? (~v p) "(point 42 1000)")
    (check-equal? (~a p) "#<point: 42 1000>")
    (check-equal? (~s p) "#<point: 42 1000>")
    (check-equal? (~v point-descriptor) "#<initialized-tuple-descriptor:point>")
    (check-equal? (~v point) "#<procedure:point>")
    (check-equal? (~v point?) "#<procedure:point?>")
    (check-equal? (~v point-x) "#<procedure:point-x>")
    (check-equal? (~v point-y) "#<procedure:point-y>")))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-tuple-type id:id (field:id ...)
    (~alt (~optional (~seq #:constructor constructor:id)
                     #:defaults ([constructor #'id]))
          (~optional (~seq #:predicate predicate:id)
                     #:defaults ([predicate
                                  (format-id #'id "~a?" (syntax-e #'id))]))
          (~optional (~seq #:property-maker property-maker:expr)
                     #:defaults ([property-maker
                                  #'make-default-tuple-properties])))
    ...)
  #:do [(define size (length (syntax->list #'(field ...))))]
  #:with quoted-size #`(quote #,size)
  #:with (field-accessor ...)
  (map (λ (field-id)
         (format-id field-id "~a-~a" (syntax-e #'id) (syntax-e field-id)))
       (syntax->list #'(field ...)))
  #:with (field-position ...) (build-list size (λ (n) #`(quote #,n)))
  (begin
    (define descriptor
      (make-tuple-implementation
       (tuple-type 'id quoted-size
                   #:constructor-name 'constructor
                   #:predicate-name 'predicate)
       #:property-maker property-maker))
    (define constructor (tuple-descriptor-constructor descriptor))
    (define predicate (tuple-descriptor-predicate descriptor))
    (define field-accessor
      (make-tuple-field-accessor descriptor field-position 'field))
    ...))
