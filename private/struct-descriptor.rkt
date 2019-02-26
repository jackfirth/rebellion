#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [initialized-struct-descriptor? (-> any/c boolean?)]
  [make-struct-type/descriptor
   (->* (#:name symbol?)
        (#:mutable-fields natural?
         #:immutable-fields natural?
         #:auto-fields natural?
         #:super-type (or/c struct-type? #f)
         #:auto-field-value any/c
         #:property-maker
         (-> uninitialized-struct-descriptor?
             (listof (cons/c struct-type-property? any/c)))
         #:inspector (or/c inspector? #f 'prefab)
         #:guard (or/c procedure? #f)
         #:constructor-name (or/c symbol? #f))
        initialized-struct-descriptor?)]
  [struct-descriptor
   (-> #:type struct-type?
       #:super-type (or/c struct-type? #f)
       #:name symbol?
       #:mutable-fields natural?
       #:immutable-fields natural?
       #:auto-fields natural?
       #:constructor procedure?
       #:predicate predicate-procedure/c
       #:accessor accessor-procedure/c
       #:mutator mutator-procedure/c
       initialized-struct-descriptor?)]
  [struct-descriptor? (-> any/c boolean?)]
  [struct-descriptor-type (-> initialized-struct-descriptor? struct-type?)]
  [uninitialized-struct-descriptor
   (-> #:super-type (or/c struct-type? #f)
       #:name symbol?
       #:mutable-fields natural?
       #:immutable-fields natural?
       #:auto-fields natural?
       #:constructor procedure?
       #:predicate predicate-procedure/c
       #:accessor accessor-procedure/c
       #:mutator mutator-procedure/c
       initialized-struct-descriptor?)]
  [uninitialized-struct-descriptor? (-> any/c boolean?)]))

(require (for-syntax racket/base
                     racket/syntax)
         racket/list
         racket/math
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define predicate-procedure/c (-> any/c boolean?))
(define accessor-procedure/c (-> any/c natural? any/c))
(define mutator-procedure/c (-> any/c natural? any/c void?))

(define (struct-descriptor? v)
  (or (initialized-struct-descriptor? v)
      (uninitialized-struct-descriptor? v)))

(define (struct-descriptor-type desc)
  (initialized-struct-descriptor-type desc))

(begin-for-syntax
  (define (format-one-identifier id template-string)
    (format-id id template-string (syntax-e id) #:source id #:props id)))


(define-simple-macro
  (define-and-provide-struct-descriptor-field-accessors
    [field:id contract:expr] ...)
  #:do [(define (format-fields template-string)
          (map (λ (field-id) (format-one-identifier field-id template-string))
               (syntax->list #'(field ...))))]
  #:with [initialized-accessor ...]
  (format-fields "initialized-struct-descriptor-~a")
  #:with [uninitialized-accessor ...]
  (format-fields "uninitialized-struct-descriptor-~a")
  #:with [generic-accessor ...] (format-fields "struct-descriptor-~a")
  (begin
    (provide
     (contract-out [generic-accessor (-> struct-descriptor? contract)] ...))
    (define (generic-accessor desc)
      (if (initialized-struct-descriptor? desc)
          (initialized-accessor desc)
          (uninitialized-accessor desc)))
    ...))

(define-and-provide-struct-descriptor-field-accessors
  [super-type (or/c struct-type? #f)]
  [name symbol?]
  [mutable-fields natural?]
  [immutable-fields natural?]
  [auto-fields natural?]
  [constructor procedure?]
  [predicate (-> any/c boolean?)]
  [accessor (-> any/c any/c any/c)]
  [mutator (-> any/c any/c any/c void?)])

(struct initialized-struct-descriptor
  (type
   super-type
   name
   mutable-fields
   immutable-fields
   auto-fields
   constructor
   predicate
   accessor
   mutator)
  #:constructor-name plain-initialized-struct-descriptor
  #:omit-define-syntaxes)

(define (struct-descriptor #:type type
                           #:super-type super-type
                           #:name name
                           #:mutable-fields mutables
                           #:immutable-fields immutables
                           #:auto-fields autos
                           #:constructor constructor
                           #:predicate predicate
                           #:accessor accessor
                           #:mutator mutator)
  (plain-initialized-struct-descriptor type
                                       super-type
                                       name
                                       mutables
                                       immutables
                                       autos
                                       constructor
                                       predicate
                                       accessor
                                       mutator))

(struct uninitialized-struct-descriptor
  (super-type
   name
   mutable-fields
   immutable-fields
   auto-fields
   constructor
   predicate
   accessor
   mutator)
  #:constructor-name plain-uninitialized-struct-descriptor
  #:omit-define-syntaxes)

(define (uninitialized-struct-descriptor
         #:super-type super-type
         #:name name
         #:mutable-fields mutables
         #:immutable-fields immutables
         #:auto-fields autos
         #:constructor constructor
         #:predicate predicate
         #:accessor accessor
         #:mutator mutator)
  (plain-uninitialized-struct-descriptor
   super-type
   name
   mutables
   immutables
   autos
   constructor
   predicate
   accessor
   mutator))

(define (make-struct-type/descriptor #:name name
                                     #:mutable-fields [mutables 0]
                                     #:immutable-fields [immutables 0]
                                     #:auto-fields [autos 0]
                                     #:super-type [super-type #f]
                                     #:auto-field-value [auto-field-value #f]
                                     #:property-maker [prop-maker (λ (_) empty)]
                                     #:inspector [inspector (current-inspector)]
                                     #:guard [guard #f]
                                     #:constructor-name [constructor-name #f])
  (define immutable-field-positions (range mutables (+ mutables immutables)))
  (define (uninitialized-constructor . vs) (apply constructor vs))
  (define (uninitialized-predicate v) (predicate v))
  (define (uninitialized-accessor this pos) (accessor this pos))
  (define (uninitialized-mutator this pos v) (mutator this pos v))
  (define uninitialized
    (uninitialized-struct-descriptor
     #:super-type super-type
     #:name name
     #:mutable-fields mutables
     #:immutable-fields immutables
     #:auto-fields autos
     #:constructor uninitialized-constructor
     #:predicate uninitialized-predicate
     #:accessor uninitialized-accessor
     #:mutator uninitialized-mutator))
  (define props (prop-maker uninitialized))
  (define-values (type constructor predicate accessor mutator)
    (make-struct-type name
                      super-type
                      (+ mutables immutables)
                      autos
                      auto-field-value
                      props
                      inspector
                      #f
                      immutable-field-positions
                      guard
                      constructor-name))
  (struct-descriptor #:type type
                     #:super-type super-type
                     #:name name
                     #:mutable-fields mutables
                     #:immutable-fields immutables
                     #:auto-fields autos
                     #:constructor constructor
                     #:predicate predicate
                     #:accessor accessor
                     #:mutator mutator))

(module+ test
  (test-case "make-struct-type/descriptor"
    (define (make-point-props descriptor)
      (define accessor (struct-descriptor-accessor descriptor))
      (define (write-proc this out _)
        (write-string "(point " out)
        (write (accessor this 0) out)
        (write-string " " out)
        (write (accessor this 1) out)
        (write-string ")" out))
      (list (cons prop:custom-write write-proc)))
    (define point-descriptor
      (make-struct-type/descriptor
       #:name 'point
       #:immutable-fields 2
       #:property-maker make-point-props))
    (define point (struct-descriptor-constructor point-descriptor))
    (define point-accessor (struct-descriptor-accessor point-descriptor))
    (define p (point 42 88))
    (check-equal? (point-accessor p 0) 42)
    (check-equal? (point-accessor p 1) 88)
    (check-equal? (~v p) "(point 42 88)")))
