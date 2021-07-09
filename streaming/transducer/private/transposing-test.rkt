#lang racket/base


(module+ test
  (require rackunit
           rebellion/base/comparator
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/transducer
           rebellion/type/record))


;@----------------------------------------------------------------------------------------------------

(module+ test
  (test-case (name-string transposing)
    (test-case "basic input with simple reducer"
      (define actual
        (transduce (list (list 1 2 3) (list 4 5 6) (list 7 8 9))
                   (transposing #:into into-list)
                   #:into into-list))
      (check-equal? actual (list (list 1 4 7) (list 2 5 8) (list 3 6 9))))))
