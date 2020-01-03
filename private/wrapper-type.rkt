#lang racket/base

(provide define-wrapper-type)

(require (for-syntax racket/base
                     racket/syntax)
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
     (~optional (~seq #:predicate-name predicate-name:id)
                #:defaults ([predicate-name (format-id #'id "~a?" #'id)])
                #:name "#:predicate-name option")
     (~optional
      (~seq #:constructor-name constructor-name:id)
      #:defaults ([constructor-name (format-id #'id "constructor:~a" #'id)])
      #:name "#:constructor-name option")
     (~optional (~seq #:accessor-name accessor-name:id)
                #:defaults ([accessor-name
                             (format-id #'id "~a-value" #'id)])
                #:name "#:accessor-name option")
     (~optional
      (~seq #:pattern-name pattern-name:id)
      #:defaults ([pattern-name (format-id #'id "pattern:~a" #'id)])
      #:name "#:pattern-name option")
     (~optional (~seq #:property-maker prop-maker:expr)
                #:defaults ([prop-maker #'default-wrapper-properties])
                #:name "#:property-maker option"))
    ...)
  #:with root-binding
  (if (attribute omit-root-binding-kw)
      #'(begin)
      #'(...
         (define-match-expander id
           (syntax-parser [(_ value-pattern) #'(pattern-name value-pattern)])
           (make-rename-transformer #'constructor-name))))
  (begin
    (define type
      (wrapper-type 'id
                    #:predicate-name 'predicate-name
                    #:constructor-name 'constructor-name
                    #:accessor-name 'accessor-name))
    (define descriptor
      (make-wrapper-implementation type #:property-maker prop-maker))
    (define predicate-name (wrapper-descriptor-predicate descriptor))
    (define constructor-name (wrapper-descriptor-constructor descriptor))
    (define accessor-name (wrapper-descriptor-accessor descriptor))
    (define-match-expander pattern-name
      (syntax-parser
        [(_ value-pattern)
         #'(? predicate-name (app accessor-name value-pattern))]))
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
