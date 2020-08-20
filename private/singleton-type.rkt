#lang racket/base

(provide define-singleton-type)

(require (for-syntax racket/base
                     racket/syntax
                     (submod rebellion/private/singleton-type-binding
                             private-constructor)
                     rebellion/type/singleton/base
                     syntax/transformer)
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
    #:attributes (default-name default-predicate-name default-descriptor-name)
    (pattern id:id
      #:do [(define (format-singleton-id fmt)
              (format-id #'id fmt #'id #:subs? #t))]
      #:with default-name #'id
      #:with default-predicate-name (format-singleton-id "~a?")
      #:with default-descriptor-name (format-singleton-id "descriptor:~a"))))

(define-simple-macro
  (define-singleton-type id:singleton-id
    (~alt
     (~optional
      (~seq #:name name:id)
      #:name "#:name option"
      #:defaults ([name #'id.default-name]))

     (~optional
      (~seq #:descriptor-name descriptor:id)
      #:name "#:descriptor-name option"
      #:defaults ([descriptor #'id.default-descriptor-name]))

     (~optional
      (~seq #:predicate-name predicate:id)
      #:name "#:predicate-name option"
      #:defaults ([predicate #'id.default-predicate-name]))

     (~optional
      (~seq #:inspector inspector:expr)
      #:name "#:inspector option"
      #:defaults ([inspector #'(current-inspector)]))

     (~optional
      (~seq #:property-maker prop-maker:expr)
      #:name "#:property-maker option"
      #:defaults
      ([prop-maker #'default-singleton-properties])))
    ...)
  (begin
    (define descriptor
      (make-singleton-implementation
       (singleton-type 'name #:predicate-name 'predicate)
       #:inspector inspector
       #:property-maker prop-maker))
    (define predicate (singleton-descriptor-predicate descriptor))
    (define instance (singleton-descriptor-instance descriptor))
    (define-syntax name
      (singleton-binding
       #:type (singleton-type 'name #:predicate-name 'predicate)
       #:descriptor #'descriptor
       #:predicate #'predicate
       #:instance #'instance
       #:macro (make-variable-like-transformer #'instance)))))

(module+ test
  (test-case "define-singleton-type"
    (define-singleton-type foo)
    (check-pred foo? foo)
    (check-false (foo? 'foo))
    (check-equal? (object-name foo) 'foo)
    (check-equal? (~a foo) "#<foo>")
    (check-equal? (~v foo) "#<foo>")
    (check-equal? (~s foo) "#<foo>")))
