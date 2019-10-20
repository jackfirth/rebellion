#lang racket/base

(module+ test
  (require rackunit
           rebellion/collection/list
           rebellion/streaming/transducer))

;@------------------------------------------------------------------------------

(module+ test
  (test-case "sorting"
    (check-equal? (transduce empty-list (sorting) #:into into-list)
                  empty-list)
    (check-equal? (transduce (list 1) (sorting) #:into into-list)
                  (list 1))))
