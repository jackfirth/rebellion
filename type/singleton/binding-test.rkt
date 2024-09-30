#lang racket/base

(module+ test
  (require (for-syntax rebellion/type/singleton/binding)
           rackunit
           rebellion/type/singleton
           syntax/parse/define))

;@------------------------------------------------------------------------------

(module+ test
  (define-singleton-type infinity)

  (test-case "basic singleton-id parsing"
    (define-syntax-parse-rule (tester :singleton-id) 'success!)
    (check-equal? (tester infinity) 'success!))
  
  (test-case "singleton-id.name"
    (define-syntax-parse-rule (tester singleton:singleton-id) singleton.name)
    (check-equal? (tester infinity) 'infinity))

  (test-case "singleton-id.descriptor"
    (define-syntax-parse-rule (tester singleton:singleton-id) singleton.descriptor)
    (check-equal? (tester infinity) descriptor:infinity))

  (test-case "singleton-id.predicate"
    (define-syntax-parse-rule (tester singleton:singleton-id) singleton.predicate)
    (check-equal? (tester infinity) infinity?))

  (test-case "singleton-id.instance"
    (define-syntax-parse-rule (tester singleton:singleton-id) singleton.instance)
    (check-equal? (tester infinity) infinity)))
