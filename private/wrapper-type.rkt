#lang racket/base

(provide define-wrapper-type)

(require (for-syntax racket/base
                     racket/syntax
                     (submod rebellion/private/wrapper-type-binding
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
           rackunit))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-wrapper-type id:id
    (~alt
     (~optional (~and #:omit-root-binding omit-root-binding-kw))

     (~optional
      (~seq #:descriptor-name descriptor:id)
      #:defaults ([descriptor (format-id #'id "descriptor:~a" #'id)])
      #:name "#:descriptor-name option")

     (~optional
      (~seq #:predicate-name predicate:id)
      #:defaults ([predicate (format-id #'id "~a?" #'id)])
      #:name "#:predicate-name option")
     
     (~optional
      (~seq #:constructor-name constructor:id)
      #:defaults ([constructor (format-id #'id "constructor:~a" #'id)])
      #:name "#:constructor-name option")
     
     (~optional
      (~seq #:accessor-name accessor:id)
      #:defaults ([accessor (format-id #'id "~a-value" #'id)])
      #:name "#:accessor-name option")
     
     (~optional
      (~seq #:pattern-name pattern:id)
      #:defaults ([pattern (format-id #'id "pattern:~a" #'id)])
      #:name "#:pattern-name option")
     
     (~optional
      (~seq #:property-maker prop-maker:expr)
      #:defaults ([prop-maker #'default-wrapper-properties])
      #:name "#:property-maker option"))
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
       #:property-maker prop-maker))
    (define predicate (wrapper-descriptor-predicate descriptor))
    (define constructor (wrapper-descriptor-constructor descriptor))
    (define accessor (wrapper-descriptor-accessor descriptor))
    (define-match-expander pattern
      (syntax-parser
        [(_ value-pattern)
         #'(? predicate (app accessor value-pattern))]))
    root-binding))

(module+ test
  (test-case "integration-test"
    (define-wrapper-type seconds)
    (check-equal? (seconds 10) (seconds 10))
    (check-not-equal? (seconds 10) (seconds 25))
    (check-equal? (constructor:seconds 10) (seconds 10))
    (check-equal? (seconds-value (seconds 10)) 10)
    (check-pred seconds? (seconds 10))
    (check-equal? (~v (seconds 10)) "(seconds 10)")
    (check-match (seconds 10) (seconds (? number?)))
    (check-match (seconds 10) (pattern:seconds (? number?)))

    (test-case "omit-root-binding option"
      (define-wrapper-type minutes #:omit-root-binding)
      (define (minutes x) (constructor:minutes x))
      (check-match (minutes 30) (pattern:minutes (? number?))))))
