#lang racket/base

(module+ test
  (require (submod "..")
           rackunit
           rebellion/collection/list
           rebellion/streaming/reducer
           rebellion/streaming/transducer))

;@------------------------------------------------------------------------------

(module+ test
  (test-case "batching"
    (check-equal?
     (transduce (in-range 10)
                (batching (reducer-limit into-list 4))
                #:into into-list)
     (list (list 0 1 2 3)
           (list 4 5 6 7)
           (list 8 9)))))
