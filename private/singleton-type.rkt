#lang racket/base

(provide define-singleton-type)

(require (for-syntax racket/base
                     racket/syntax)
         rebellion/type/singleton/base
         rebellion/type/singleton/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class singleton-id
    #:attributes (default-name
                  default-predicate-name
                  default-descriptor-name
                  default-type-name)
    (pattern id:id
      #:do [(define (format-singleton-id fmt)
              (format-id #'id fmt #'id #:source #'id #:props #'id))]
      #:with default-name #'id
      #:with default-predicate-name (format-singleton-id "~a?")
      #:with default-descriptor-name (format-singleton-id "descriptor:~a")
      #:with default-type-name (format-singleton-id "type:~a"))))

(define-simple-macro
  (define-singleton-type id:singleton-id
    (~alt (~optional (~seq #:name name:id)
                     #:name "#:name option"
                     #:defaults ([name #'id.default-name]))

          (~optional (~seq #:predicate-name predicate:id)
                     #:name "#:predicate-name option"
                     #:defaults ([predicate #'id.default-predicate-name]))

          (~optional (~seq #:descriptor-name descriptor:id)
                     #:name "#:descriptor-name option"
                     #:defaults ([descriptor #'id.default-descriptor-name]))

          (~optional (~seq #:type-representation-name type:id)
                     #:name "#:type-representation-name option"
                     #:defaults ([type #'id.default-type-name]))

          (~optional (~seq #:inspector inspector:expr)
                     #:name "#:inspector option"
                     #:defaults ([inspector #'(current-inspector)]))

          (~optional (~seq #:property-maker prop-maker:expr)
                     #:name "#:property-maker option"
                     #:defaults
                     ([prop-maker #'default-singleton-properties])))
    ...)
  (begin
    (define type (singleton-type 'name #:predicate-name 'predicate))
    (define descriptor
      (make-singleton-implementation
       type #:inspector inspector #:property-maker prop-maker))
    (define name (singleton-descriptor-instance descriptor))
    (define predicate (singleton-descriptor-predicate descriptor))))

(module+ test
  (test-case "define-singleton-type"
    (define-singleton-type foo)
    (check-pred foo? foo)
    (check-false (foo? 'foo))
    (check-equal? (object-name foo) 'foo)
    (check-equal? (~a foo) "#<foo>")
    (check-equal? (~v foo) "#<foo>")
    (check-equal? (~s foo) "#<foo>")))
