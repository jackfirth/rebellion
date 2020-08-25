#lang racket/base

(provide define-singleton-type
         singleton-out)

(require (for-syntax racket/base
                     racket/provide-transform
                     racket/syntax
                     (submod rebellion/private/singleton-type-binding
                             private-constructor)
                     rebellion/type/singleton/base
                     rebellion/type/singleton/binding
                     syntax/parse
                     syntax/transformer)
         rebellion/type/singleton/base
         rebellion/type/singleton/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-singleton-type id:id
    (~alt
     (~optional
      (~seq #:descriptor-name descriptor:id)
      #:name "#:descriptor-name option"
      #:defaults
      ([descriptor (format-id #'id "descriptor:~a" #'id #:subs? #t)]))

     (~optional
      (~seq #:predicate-name predicate:id)
      #:name "#:predicate-name option"
      #:defaults ([predicate (format-id #'id "~a?" #'id #:subs? #t)]))

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
       (singleton-type 'id #:predicate-name 'predicate)
       #:inspector inspector
       #:property-maker prop-maker))
    (define predicate (singleton-descriptor-predicate descriptor))
    (define instance (singleton-descriptor-instance descriptor))
    (define-syntax id
      (singleton-binding
       #:type (singleton-type 'id #:predicate-name 'predicate)
       #:descriptor #'descriptor
       #:predicate #'predicate
       #:instance #'instance
       #:macro (make-variable-like-transformer #'instance)))))

(module+ test
  (test-case (name-string define-singleton-type)
    (define-singleton-type foo)
    (check-pred foo? foo)
    (check-false (foo? 'foo))
    (check-equal? (object-name foo) 'foo)
    (check-equal? (~a foo) "#<foo>")
    (check-equal? (~v foo) "#<foo>")
    (check-equal? (~s foo) "#<foo>")))

(define-syntax singleton-out
  (make-provide-transformer
   (λ (provide-spec modes)
     (syntax-parse provide-spec
       [(_ singleton:singleton-id)
        (expand-export
         #'(combine-out singleton singleton.predicate) modes)]))))

(module+ test
  (provide (singleton-out null))
  (define-singleton-type null))
