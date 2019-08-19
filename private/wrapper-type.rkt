#lang racket/base

(require racket/contract/base)

(provide
 define-wrapper-type)

(require (for-syntax racket/base
                     racket/syntax)
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
    (~alt (~optional (~seq #:predicate-name predicate-name:id)
                     #:defaults ([predicate-name (format-id #'id "~a?" #'id)])
                     #:name "#:predicate-name option")
          (~optional (~seq #:constructor-name constructor-name:id)
                     #:defaults ([constructor-name #'id])
                     #:name "#:constructor-name option")
          (~optional (~seq #:accessor-name accessor-name:id)
                     #:defaults ([accessor-name
                                  (format-id #'id "~a-value" #'id)])
                     #:name "#:accessor-name option")
          (~optional (~seq #:property-maker prop-maker:expr)
                     #:defaults ([prop-maker #'make-default-wrapper-properties])
                     #:name "#:property-maker option"))
    ...)
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
    (define accessor-name (wrapper-descriptor-accessor descriptor))))

(module+ test
  (test-case "integration-test"
    (define-wrapper-type seconds)
    (check-equal? (seconds 10) (seconds 10))
    (check-not-equal? (seconds 10) (seconds 25))
    (check-equal? (seconds-value (seconds 10)) 10)
    (check-pred seconds? (seconds 10))
    (check-equal? (~v (seconds 10)) "(seconds 10)")))
