#lang racket/base

(module+ test
  (require (for-syntax rebellion/type/object/binding)
           rackunit
           rebellion/type/object
           syntax/parse/define))

;@------------------------------------------------------------------------------

(module+ test
  (define-object-type widget (price weight description))

  (test-case "basic object-id parsing"
    (define-syntax-parse-rule (tester :object-id) 'success!)
    (check-equal? (tester widget) 'success!))
  
  (test-case "object-id.name"
    (define-syntax-parse-rule (tester object:object-id) object.name)
    (check-equal? (tester widget) 'widget))

  (test-case "object-id.descriptor"
    (define-syntax-parse-rule (tester object:object-id) object.descriptor)
    (check-equal? (tester widget) descriptor:widget))

  (test-case "object-id.predicate"
    (define-syntax-parse-rule (tester object:object-id) object.predicate)
    (check-equal? (tester widget) widget?))

  (test-case "object-id.constructor"
    (define-syntax-parse-rule (tester object:object-id) object.constructor)
    (check-equal? (tester widget) make-widget))

  (test-case "object-id.accessor"
    (define-syntax-parse-rule (tester object:object-id) object.accessor)
    (check-equal? (tester widget) accessor:widget))

  (test-case "object-id.field"
    (define-syntax-parse-rule (tester object:object-id) (list 'object.field ...))
    (check-equal? (tester widget) (list 'description 'name 'price 'weight)))

  (test-case "object-id.field-name"
    (define-syntax-parse-rule (tester object:object-id) (list object.field-name ...))
    (check-equal? (tester widget) (list 'description 'name 'price 'weight)))

  (test-case "object-id.field-keyword"
    (define-syntax-parse-rule (tester object:object-id)
      (list 'object.field-keyword ...))
    (check-equal?
     (tester widget) (list '#:description '#:name '#:price '#:weight)))

  (test-case "object-id.field-accessor"
    (define-syntax-parse-rule (tester object:object-id)
      (list object.field-accessor ...))
    (check-equal?
     (tester widget)
     (list widget-description widget-name widget-price widget-weight)))

  (test-case "object-id.private-field"
    (define-syntax-parse-rule (tester object:object-id)
      (list 'object.private-field ...))
    (check-equal? (tester widget) (list 'description 'price 'weight)))

  (test-case "object-id.private-field-name"
    (define-syntax-parse-rule (tester object:object-id)
      (list object.private-field-name ...))
    (check-equal? (tester widget) (list 'description 'price 'weight)))

  (test-case "object-id.private-field-keyword"
    (define-syntax-parse-rule (tester object:object-id)
      (list 'object.private-field-keyword ...))
    (check-equal? (tester widget) (list '#:description '#:price '#:weight)))

  (test-case "object-id.private-accessor"
    (define-syntax-parse-rule (tester object:object-id)
      (list object.private-accessor ...))
    (check-equal?
     (tester widget) (list widget-description widget-price widget-weight)))

  (test-case "object-id.name-field"
    (define-syntax-parse-rule (tester object:object-id) 'object.name-field)
    (check-equal? (tester widget) 'name))
  
  (test-case "object-id.name-field-name"
    (define-syntax-parse-rule (tester object:object-id) object.name-field-name)
    (check-equal? (tester widget) 'name))
  
  (test-case "object-id.name-field-keyword"
    (define-syntax-parse-rule (tester object:object-id) 'object.name-field-keyword)
    (check-equal? (tester widget) '#:name))

  (test-case "object-id.name-accessor"
    (define-syntax-parse-rule (tester object:object-id) object.name-accessor)
    (check-equal? (tester widget) widget-name)))
