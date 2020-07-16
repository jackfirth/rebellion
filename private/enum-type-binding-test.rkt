#lang racket/base

(module+ test
  (require (for-syntax racket/base
                       rebellion/collection/keyset
                       rebellion/private/enum-type-binding
                       rebellion/type/enum)
           racket/set
           rackunit
           rebellion/collection/keyset
           rebellion/type/enum
           syntax/parse/define))

;@------------------------------------------------------------------------------

(module+ test
  (define-enum-type direction (up down left right))

  (test-case "basic enum-id parsing"
    (define-simple-macro (tester :enum-id) 'success!)
    (check-equal? (tester direction) 'success!))
  
  (test-case "enum-id.name"
    (define-simple-macro (tester enum:enum-id) enum.name)
    (check-equal? (tester direction) 'direction))
  
  (test-case "enum-id.constant"
    (define-simple-macro (tester enum:enum-id) (set enum.constant ...))
    (check-equal? (tester direction) (set up down left right)))
  
  (test-case "enum-id.predicate"
    (define-simple-macro (tester enum:enum-id) enum.predicate)
    (check-equal? (tester direction) direction?))
  
  (test-case "enum-id.discriminator"
    (define-simple-macro (tester enum:enum-id) enum.discriminator)
    (check-equal? (tester direction) discriminator:direction))
  
  (test-case "enum-id.selector"
    (define-simple-macro (tester enum:enum-id) enum.selector)
    (check-equal? (tester direction) selector:direction))
  
  (test-case "enum-id.descriptor"
    (define-simple-macro (tester enum:enum-id) enum.descriptor)
    (check-equal? (tester direction) descriptor:direction)))
