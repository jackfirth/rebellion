#lang racket/base

(module+ test
  (require (for-syntax rebellion/type/enum/binding)
           rackunit
           rebellion/type/enum
           syntax/parse/define))

;@------------------------------------------------------------------------------

(module+ test
  (define-enum-type direction (up down left right))

  (test-case "basic enum-id parsing"
    (define-syntax-parse-rule (tester :enum-id) 'success!)
    (check-equal? (tester direction) 'success!))
  
  (test-case "enum-id.name"
    (define-syntax-parse-rule (tester enum:enum-id) enum.name)
    (check-equal? (tester direction) 'direction))
  
  (test-case "enum-id.constant"
    (define-syntax-parse-rule (tester enum:enum-id) (list enum.constant ...))
    (check-equal? (tester direction) (list down left right up)))

  (test-case "enum-id.constant-name"
    (define-syntax-parse-rule (tester enum:enum-id) (list enum.constant-name ...))
    (check-equal? (tester direction) (list 'down 'left 'right 'up)))
  
  (test-case "enum-id.predicate"
    (define-syntax-parse-rule (tester enum:enum-id) enum.predicate)
    (check-equal? (tester direction) direction?))
  
  (test-case "enum-id.discriminator"
    (define-syntax-parse-rule (tester enum:enum-id) enum.discriminator)
    (check-equal? (tester direction) discriminator:direction))
  
  (test-case "enum-id.selector"
    (define-syntax-parse-rule (tester enum:enum-id) enum.selector)
    (check-equal? (tester direction) selector:direction))
  
  (test-case "enum-id.descriptor"
    (define-syntax-parse-rule (tester enum:enum-id) enum.descriptor)
    (check-equal? (tester direction) descriptor:direction)))
