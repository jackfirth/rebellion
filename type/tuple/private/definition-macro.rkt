#lang racket/base

(provide define-tuple-type)

(require (for-syntax racket/base
                     racket/sequence
                     rebellion/type/private/naming
                     (submod rebellion/type/tuple/binding
                             private-constructor)
                     rebellion/type/tuple/base
                     syntax/transformer)
         racket/match
         rebellion/type/tuple/base
         rebellion/type/tuple/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-tuple-type id:id (field:id ...)
    (~alt

     (~optional (~and #:omit-root-binding omit-root-binding-kw))

     (~optional
      (~seq #:descriptor-name descriptor:id)
      #:defaults ([descriptor (default-descriptor-identifier #'id)])
      #:name "#:descriptor-name option")
     
     (~optional
      (~seq #:predicate-name predicate:id)
      #:defaults ([predicate (default-predicate-identifier #'id)])
      #:name "#:predicate-name option")
     
     (~optional
      (~seq #:constructor-name constructor:id)
      #:defaults ([constructor (default-constructor-identifier #'id)])
      #:name "#:constructor-name option")
     
     (~optional
      (~seq #:accessor-name accessor:id)
      #:defaults ([accessor (default-accessor-identifier #'id)])
      #:name "#:accessor-name option")
     
     (~optional
      (~seq #:pattern-name pattern:id)
      #:defaults ([pattern (default-pattern-identifier #'id)])
      #:name "#:pattern-name option")

     (~optional
      (~seq #:inspector inspector:expr)
      #:name "#:inspector option"
      #:defaults ([inspector #'(current-inspector)]))
     
     (~optional
      (~seq #:property-maker property-maker:expr)
      #:defaults ([property-maker #'default-tuple-properties])
      #:name "#:property-maker option"))
    ...)
  
  #:do [(define size (length (syntax->list #'(field ...))))]

  #:with (field-accessor ...)
  (for/list ([field-id (in-syntax #'(field ...))])
    (default-field-accessor-identifier #'id field-id))
  
  #:with (field-position ...) (build-list size (λ (n) #`(quote #,n)))

  #:with root-binding
  (if (attribute omit-root-binding-kw)
      #'(begin)
      #'(define-syntax id
          (tuple-binding
           #:type
           (tuple-type
            'id (list 'field ...)
            #:predicate-name 'predicate
            #:accessor-name 'accessor
            #:constructor-name 'constructor)
           #:descriptor #'descriptor
           #:predicate #'predicate
           #:constructor #'constructor
           #:accessor #'accessor
           #:fields (list #'field ...)
           #:field-accessors (list #'field-accessor ...)
           #:pattern #'pattern
           #:macro (make-variable-like-transformer #'constructor))))
  
  (begin
    (define descriptor
      (make-tuple-implementation
       (tuple-type
        'id (list 'field ...)
        #:predicate-name 'predicate
        #:accessor-name 'accessor
        #:constructor-name 'constructor)
       #:inspector inspector
       #:property-maker property-maker))
    (define constructor (tuple-descriptor-constructor descriptor))
    (define predicate (tuple-descriptor-predicate descriptor))
    (define accessor (tuple-descriptor-accessor descriptor))
    (define field-accessor
      (make-tuple-field-accessor descriptor field-position))
    ...
    (define-match-expander pattern
      (syntax-parser
        [(_ field ...) #'(? predicate (app field-accessor field) ...)]))
    root-binding))

(module+ test
  (test-case (name-string define-tuple-type)
    (define-tuple-type point (x y))
    (check-pred point? (point 1 2))
    (check-equal? (point-x (point 1 2)) 1)
    (check-equal? (point-y (point 1 2)) 2)
    (check-equal? (point 1 2) (point 1 2))
    (check-pred point? (apply point (list 1 2)))
    (check-match (point 1 2) (point x _) (equal? x 1))
    (check-match (point 1 2) (point _ y) (equal? y 2))

    (test-case "should allow omitting the root binding"
      (define-tuple-type point (x y) #:omit-root-binding)
      (check-match (constructor:point 1 2) (pattern:point x y)
                   (and (equal? x 1) (equal? y 2)))
      (check-pred point? (apply constructor:point (list 1 2))))))
