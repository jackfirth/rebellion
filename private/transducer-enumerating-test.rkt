#lang racket/base

(module+ test
  (require (submod "..")
           rackunit
           rebellion/collection/list
           rebellion/streaming/transducer))

;@------------------------------------------------------------------------------

(module+ test
  (test-case "enumerating"
    (check-equal? (transduce "cat" enumerating #:into into-list)
                  (list (enumerated #:element #\c #:position 0)
                        (enumerated #:element #\a #:position 1)
                        (enumerated #:element #\t #:position 2)))))
