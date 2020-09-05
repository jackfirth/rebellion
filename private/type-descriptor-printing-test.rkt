#lang racket

(module+ test
  (require racket/format
           rackunit
           rebellion/type/enum
           rebellion/type/object
           rebellion/type/record
           rebellion/type/singleton
           rebellion/type/tuple
           rebellion/type/wrapper))

(module+ test
  (test-case "tuple descriptor printing"
    (define-tuple-type foo (a b))
    (check-equal? (object-name descriptor:foo) 'foo)
    (check-equal? (~a descriptor:foo) "#<tuple-descriptor:foo>")
    (check-equal? (~v descriptor:foo) "#<tuple-descriptor:foo>")
    (check-equal? (~s descriptor:foo) "#<tuple-descriptor:foo>")
    (check-equal? (~v (list descriptor:foo)) "(list #<tuple-descriptor:foo>)"))

  (test-case "record descriptor printing"
    (define-record-type foo (a b))
    (check-equal? (object-name descriptor:foo) 'foo)
    (check-equal? (~a descriptor:foo) "#<record-descriptor:foo>")
    (check-equal? (~v descriptor:foo) "#<record-descriptor:foo>")
    (check-equal? (~s descriptor:foo) "#<record-descriptor:foo>")
    (check-equal? (~v (list descriptor:foo)) "(list #<record-descriptor:foo>)"))

  (test-case "wrapper descriptor printing"
    (define-wrapper-type foo)
    (check-equal? (object-name descriptor:foo) 'foo)
    (check-equal? (~a descriptor:foo) "#<wrapper-descriptor:foo>")
    (check-equal? (~v descriptor:foo) "#<wrapper-descriptor:foo>")
    (check-equal? (~s descriptor:foo) "#<wrapper-descriptor:foo>")
    (check-equal?
     (~v (list descriptor:foo)) "(list #<wrapper-descriptor:foo>)"))

  (test-case "singleton descriptor printing"
    (define-singleton-type foo)
    (check-equal? (object-name descriptor:foo) 'foo)
    (check-equal? (~a descriptor:foo) "#<singleton-descriptor:foo>")
    (check-equal? (~v descriptor:foo) "#<singleton-descriptor:foo>")
    (check-equal? (~s descriptor:foo) "#<singleton-descriptor:foo>")
    (check-equal?
     (~v (list descriptor:foo)) "(list #<singleton-descriptor:foo>)"))

  (test-case "enum descriptor printing"
    (define-enum-type foo (a b))
    (check-equal? (object-name descriptor:foo) 'foo)
    (check-equal? (~a descriptor:foo) "#<enum-descriptor:foo>")
    (check-equal? (~v descriptor:foo) "#<enum-descriptor:foo>")
    (check-equal? (~s descriptor:foo) "#<enum-descriptor:foo>")
    (check-equal? (~v (list descriptor:foo)) "(list #<enum-descriptor:foo>)"))

  (test-case "object descriptor printing"
    (define-object-type foo (a b))
    (check-equal? (object-name descriptor:foo) 'foo)
    (check-equal? (~a descriptor:foo) "#<object-descriptor:foo>")
    (check-equal? (~v descriptor:foo) "#<object-descriptor:foo>")
    (check-equal? (~s descriptor:foo) "#<object-descriptor:foo>")
    (check-equal?
     (~v (list descriptor:foo)) "(list #<object-descriptor:foo>)")))
