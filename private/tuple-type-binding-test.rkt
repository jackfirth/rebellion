#lang racket/base

(module+ test
  (require (for-syntax rebellion/type/tuple/binding)
           rackunit
           rebellion/type/tuple
           syntax/parse/define))

;@------------------------------------------------------------------------------

(module+ test
  (define-tuple-type widget (price weight description))

  (test-case "basic tuple-id parsing"
    (define-simple-macro (tester :tuple-id) 'success!)
    (check-equal? (tester widget) 'success!))
  
  (test-case "tuple-id.name"
    (define-simple-macro (tester tuple:tuple-id) tuple.name)
    (check-equal? (tester widget) 'widget))

  (test-case "tuple-id.descriptor"
    (define-simple-macro (tester tuple:tuple-id) tuple.descriptor)
    (check-equal? (tester widget) descriptor:widget))

  (test-case "tuple-id.predicate"
    (define-simple-macro (tester tuple:tuple-id) tuple.predicate)
    (check-equal? (tester widget) widget?))

  (test-case "tuple-id.constructor"
    (define-simple-macro (tester tuple:tuple-id) tuple.constructor)
    (check-equal? (tester widget) widget))

  (test-case "tuple-id.accessor"
    (define-simple-macro (tester tuple:tuple-id) tuple.accessor)
    (check-equal? (tester widget) accessor:widget))

  (test-case "tuple-id.field"
    (define-simple-macro (tester tuple:tuple-id) (list 'tuple.field ...))
    (check-equal? (tester widget) (list 'price 'weight 'description)))

  (test-case "tuple-id.field-name"
    (define-simple-macro (tester tuple:tuple-id) (list tuple.field-name ...))
    (check-equal? (tester widget) (list 'price 'weight 'description)))

  (test-case "tuple-id.field-accessor"
    (define-simple-macro (tester tuple:tuple-id)
      (list tuple.field-accessor ...))
    (check-equal?
     (tester widget) (list widget-price widget-weight widget-description))))
