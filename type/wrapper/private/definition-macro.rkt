#lang racket/base

(provide define-wrapper-type)

(require (for-syntax racket/base
                     rebellion/type/private/naming
                     (submod rebellion/type/wrapper/binding
                             private-constructor)
                     rebellion/type/wrapper/base
                     syntax/transformer)
         racket/match
         rebellion/type/wrapper/base
         rebellion/type/wrapper/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-wrapper-type id:id
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
      #:defaults ([accessor (default-unwrapping-accessor-identifier #'id)])
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
      (~seq #:property-maker prop-maker:expr)
      #:defaults ([prop-maker #'default-wrapper-properties])
      #:name "#:property-maker option")

     (~optional
      (~seq #:guard-maker guard-maker:expr)
      #:name "#:guard-maker option"
      #:defaults ([guard-maker #'#f])))
    ...)
  
  #:with root-binding
  (if (attribute omit-root-binding-kw)
      #'(begin)
      #'(define-syntax id
          (wrapper-binding
           #:type
           (wrapper-type
            'id
            #:constructor-name 'constructor
            #:predicate-name 'predicate
            #:accessor-name 'accessor)
           #:descriptor #'descriptor
           #:predicate #'predicate
           #:constructor #'constructor
           #:accessor #'accessor
           #:pattern #'pattern
           #:macro (make-variable-like-transformer #'constructor))))
  
  (begin
    (define descriptor
      (make-wrapper-implementation
       (wrapper-type
        'id
        #:predicate-name 'predicate
        #:constructor-name 'constructor
        #:accessor-name 'accessor)
       #:inspector inspector
       #:property-maker prop-maker
       #:guard-maker guard-maker))
    (define predicate (wrapper-descriptor-predicate descriptor))
    (define constructor (wrapper-descriptor-constructor descriptor))
    (define accessor (wrapper-descriptor-accessor descriptor))
    (define-match-expander pattern
      (syntax-parser
        [(_ value-pattern)
         #'(? predicate (app accessor value-pattern))]))
    root-binding))

(module+ test
  (test-case (name-string define-wrapper-type)
    (define-wrapper-type seconds)
    (check-equal? (seconds 10) (seconds 10))
    (check-not-equal? (seconds 10) (seconds 25))
    (check-equal? (constructor:seconds 10) (seconds 10))
    (check-equal? (seconds-value (seconds 10)) 10)
    (check-pred seconds? (seconds 10))
    (check-equal? (~v (seconds 10)) "(seconds 10)")
    (check-match (seconds 10) (seconds (? number?)))
    (check-match (seconds 10) (pattern:seconds (? number?)))

    (test-case "should allow omitting the root binding"
      (define-wrapper-type minutes #:omit-root-binding)
      (define (minutes x) (constructor:minutes x))
      (check-match (minutes 30) (pattern:minutes (? number?))))))
