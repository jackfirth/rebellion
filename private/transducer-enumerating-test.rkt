#lang racket/base

(module+ test
  (require rackunit
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))

;@------------------------------------------------------------------------------

(module+ test
  (test-case (name-string enumerating)
    (define inputs "cat")
    (define expected
      (list (enumerated #:element #\c #:position 0)
            (enumerated #:element #\a #:position 1)
            (enumerated #:element #\t #:position 2)))
    (define expected-events
      (list start-event
            (consume-event #\c)
            (emit-event (enumerated #:element #\c #:position 0))
            (consume-event #\a)
            (emit-event (enumerated #:element #\a #:position 1))
            (consume-event #\t)
            (emit-event (enumerated #:element #\t #:position 2))
            half-close-event
            finish-event))
    (check-equal? (transduce inputs enumerating #:into into-list) expected)
    (define actual-events
      (transduce inputs
                 (observing-transduction-events enumerating)
                 #:into into-list))
    (check-equal? actual-events expected-events)))
