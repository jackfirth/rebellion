#lang racket/base

(module+ test
  (require (for-syntax rebellion/type/wrapper/binding)
           rackunit
           rebellion/type/wrapper
           syntax/parse/define))

;@------------------------------------------------------------------------------

(module+ test
  (define-wrapper-type radians)

  (test-case "basic wrapper-id parsing"
    (define-simple-macro (tester :wrapper-id) 'success!)
    (check-equal? (tester radians) 'success!))
  
  (test-case "wrapper-id.name"
    (define-simple-macro (tester wrapper:wrapper-id) wrapper.name)
    (check-equal? (tester radians) 'radians))

  (test-case "wrapper-id.descriptor"
    (define-simple-macro (tester wrapper:wrapper-id) wrapper.descriptor)
    (check-equal? (tester radians) descriptor:radians))

  (test-case "wrapper-id.predicate"
    (define-simple-macro (tester wrapper:wrapper-id) wrapper.predicate)
    (check-equal? (tester radians) radians?))

  (test-case "wrapper-id.constructor"
    (define-simple-macro (tester wrapper:wrapper-id) wrapper.constructor)
    (check-equal? (tester radians) radians))

  (test-case "wrapper-id.accessor"
    (define-simple-macro (tester wrapper:wrapper-id) wrapper.accessor)
    (check-equal? (tester radians) radians-value)))
