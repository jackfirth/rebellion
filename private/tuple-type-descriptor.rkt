#lang racket/base

(require racket/contract/base)

(provide
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
             (listof (cons/c struct-type-property? any/c))))
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
         rebellion/collection/keyset/low-dependency
         rebellion/custom-write
         rebellion/equal+hash
         rebellion/equal+hash/struct
         rebellion/private/impersonation
         rebellion/private/struct-definition-util
         rebellion/type/tuple/base
         rebellion/type/struct)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

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
        (cons prop:custom-write (default-tuple-custom-write descriptor))))

;@------------------------------------------------------------------------------

(define fields:uninitialized-tuple-descriptor
  (keyset #:type #:predicate #:constructor #:accessor))

(define fields:initialized-tuple-descriptor
  (keyset #:type #:predicate #:constructor #:accessor #:backing-struct-type))

(define descriptor:uninitialized-tuple-descriptor
  (make-struct-implementation
   #:name 'uninitialized-tuple-descriptor
   #:immutable-fields (keyset-size fields:uninitialized-tuple-descriptor)
   #:constructor-name 'constructor:uninitialized-tuple-descriptor
   #:property-maker make-descriptor-style-properties))

(define descriptor:initialized-tuple-descriptor
  (make-struct-implementation
   #:name 'initialized-tuple-descriptor
   #:immutable-fields (keyset-size fields:initialized-tuple-descriptor)
   #:constructor-name 'constructor:initialized-tuple-descriptor
   #:property-maker make-descriptor-style-properties))

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
                                      #:backing-struct-type struct-type
                                      #:predicate predicate
                                      #:constructor constructor
                                      #:accessor accessor)
  (constructor:initialized-tuple-descriptor
   type predicate constructor accessor struct-type))

(define-struct-field-accessors initialized-tuple-descriptor
  (type predicate constructor accessor backing-struct-type)
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
         #:property-maker [prop-maker default-tuple-properties])
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
   #:constructor (get-constructor descriptor)
   #:accessor (get-accessor descriptor)))

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
    (check-equal? (~v point-descriptor) "#<initialized-tuple-descriptor:point>")
    (check-equal? (~v point) "#<procedure:point>")
    (check-equal? (~v point?) "#<procedure:point?>")
    (check-equal? (~v point-x) "#<procedure:point-x>")
    (check-equal? (~v point-y) "#<procedure:point-y>")))
