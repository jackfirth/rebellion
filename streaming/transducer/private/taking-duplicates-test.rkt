#lang racket/base

(module+ test
  (require rackunit
           rebellion/base/immutable-string
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))

;@------------------------------------------------------------------------------

(module+ test
  (test-case (name-string taking-duplicates)
    (test-case "should compare entire values by default"
      (define trans (taking-duplicates))
      (define inputs "hello world")
      (define expected "lol")
      (check-equal? (transduce inputs trans #:into into-string) expected)
      (define actual-events (transduce inputs (observing-transduction-events trans) #:into into-list))
      (define expected-events
        (list start-event
              (consume-event #\h)
              (consume-event #\e)
              (consume-event #\l)
              (consume-event #\l)
              (emit-event #\l)
              (consume-event #\o)
              (consume-event #\space)
              (consume-event #\w)
              (consume-event #\o)
              (emit-event #\o)
              (consume-event #\r)
              (consume-event #\l)
              (emit-event #\l)
              (consume-event #\d)
              half-close-event
              finish-event))
      (check-equal? actual-events expected-events))

    (test-case "should compare elements using given key function"
      (define trans (taking-duplicates #:key immutable-string-foldcase))
      (define inputs (list "foo" "FOO" "Bar" "bar" "Foo"))
      (define expected (list "FOO" "bar" "Foo"))
      (check-equal? (transduce inputs trans #:into into-list) expected)
      (define actual-events (transduce inputs (observing-transduction-events trans) #:into into-list))
      (define expected-events
        (list start-event
              (consume-event "foo")
              (consume-event "FOO")
              (emit-event "FOO")
              (consume-event "Bar")
              (consume-event "bar")
              (emit-event "bar")
              (consume-event "Foo")
              (emit-event "Foo")
              half-close-event
              finish-event))
      (check-equal? actual-events expected-events))

    (test-case "should emit nothing when upstream sequence empty"
      (define trans (taking-duplicates))
      (check-equal? (transduce empty-list trans #:into into-list) empty-list)
      (define actual-events
        (transduce empty-list (observing-transduction-events trans) #:into into-list))
      (define expected-events (list start-event half-close-event finish-event))
      (check-equal? actual-events expected-events))))
