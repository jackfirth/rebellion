#lang racket/base

(module+ test
  (require (for-syntax rebellion/type/record/binding)
           rackunit
           rebellion/type/record
           syntax/parse/define))

;@------------------------------------------------------------------------------

(module+ test
  (define-record-type widget (price weight description))

  (test-case "basic record-id parsing"
    (define-simple-macro (tester :record-id) 'success!)
    (check-equal? (tester widget) 'success!))
  
  (test-case "record-id.name"
    (define-simple-macro (tester record:record-id) record.name)
    (check-equal? (tester widget) 'widget))

  (test-case "record-id.field-name"
    (define-simple-macro (tester record:record-id) (list record.field-name ...))
    (check-equal? (tester widget) (list 'description 'price 'weight)))

  (test-case "record-id.descriptor"
    (define-simple-macro (tester record:record-id) record.descriptor)
    (check-equal? (tester widget) descriptor:widget))

  (test-case "record-id.predicate"
    (define-simple-macro (tester record:record-id) record.predicate)
    (check-equal? (tester widget) widget?))

  (test-case "record-id.constructor"
    (define-simple-macro (tester record:record-id) record.constructor)
    (check-equal? (tester widget) widget))

  (test-case "record-id.accessor"
    (define-simple-macro (tester record:record-id) record.accessor)
    (check-equal? (tester widget) accessor:widget))

  (test-case "record-id.field-accessor"
    (define-simple-macro (tester record:record-id)
      (list record.field-accessor ...))
    (check-equal?
     (tester widget) (list widget-description widget-price widget-weight))))
