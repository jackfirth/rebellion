#lang racket/base

(module+ test
  (require rackunit
           rebellion/base/option
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer))

;@------------------------------------------------------------------------------

(module+ test
  (test-case (name-string into-transduced)

    (test-case "should finish early when reducer finishes early"
      (define red (into-transduced (mapping -) #:into into-first))
      (check-equal? (reduce red 1 2 3) (present -1))
      (check-equal? (reduce red) absent))

    (test-case "should finish early when transducer finishes"
      (define red (into-transduced (taking 3) #:into into-list))
      (check-equal? (reduce-all red (in-range 1 10)) (list 1 2 3))
      (check-equal? (reduce red 1 2) (list 1 2)))

    (test-case "should finish normally when neither finishes early"
      (define red (into-transduced (mapping -) #:into into-list))
      (check-equal? (reduce red 1 2 3) (list -1 -2 -3)))))
