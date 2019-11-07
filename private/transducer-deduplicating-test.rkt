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
  (test-case (name-string deduplicating)
    (test-case "should compare entire values by default"
      (define trans (deduplicating))
      (define inputs "hello world")
      (define expected "helo wrd")
      (check-equal? (transduce inputs trans #:into into-string) expected)
      (check-equal? (transduce inputs (materializing trans) #:into into-list)
                    (list start-event
                          (consume-event #\h)
                          (emit-event #\h)
                          (consume-event #\e)
                          (emit-event #\e)
                          (consume-event #\l)
                          (emit-event #\l)
                          (consume-event #\l)
                          (consume-event #\o)
                          (emit-event #\o)
                          (consume-event #\space)
                          (emit-event #\space)
                          (consume-event #\w)
                          (emit-event #\w)
                          (consume-event #\o)
                          (consume-event #\r)
                          (emit-event #\r)
                          (consume-event #\l)
                          (consume-event #\d)
                          (emit-event #\d)
                          half-close-event
                          finish-event)))

    (test-case "should remove every duplicate"
      (define trans (deduplicating))
      (check-equal? (transduce "zzzzz" trans #:into into-string) "z")
      (check-equal? (transduce "zzzzz" (materializing trans) #:into into-list)
                    (list start-event
                          (consume-event #\z)
                          (emit-event #\z)
                          (consume-event #\z)
                          (consume-event #\z)
                          (consume-event #\z)
                          (consume-event #\z)
                          half-close-event
                          finish-event)))

    (test-case "should compare elements using given key function"
      (define trans (deduplicating #:key immutable-string-foldcase))
      (define inputs (list "foo" "FOO" "Bar" "bar" "Foo"))
      (define expected (list "foo" "Bar"))
      (check-equal? (transduce inputs trans #:into into-list) expected)
      (check-equal? (transduce inputs (materializing trans) #:into into-list)
                    (list start-event
                          (consume-event "foo")
                          (emit-event "foo")
                          (consume-event "FOO")
                          (consume-event "Bar")
                          (emit-event "Bar")
                          (consume-event "bar")
                          (consume-event "Foo")
                          half-close-event
                          finish-event)))

    (test-case "should emit nothing when upstream sequence empty"
      (define trans (deduplicating))
      (check-equal? (transduce empty-list trans #:into into-list) empty-list)
      (check-equal?
       (transduce empty-list (materializing trans) #:into into-list)
       (list start-event half-close-event finish-event))))

  (test-case (name-string deduplicating-consecutive)
    (test-case "should compare entire values by default"
      (define trans (deduplicating-consecutive))
      (define inputs (list 2 2 2 2 3 7 7 7 3 3 1 1 7 7))
      (define expected (list 2 3 7 3 1 7))
      (check-equal? (transduce inputs trans #:into into-list) expected)
      (check-equal? (transduce inputs (materializing trans) #:into into-list)
                    (list start-event
                          (consume-event 2)
                          (emit-event 2)
                          (consume-event 2)
                          (consume-event 2)
                          (consume-event 2)
                          (consume-event 3)
                          (emit-event 3)
                          (consume-event 7)
                          (emit-event 7)
                          (consume-event 7)
                          (consume-event 7)
                          (consume-event 3)
                          (emit-event 3)
                          (consume-event 3)
                          (consume-event 1)
                          (emit-event 1)
                          (consume-event 1)
                          (consume-event 7)
                          (emit-event 7)
                          (consume-event 7)
                          half-close-event
                          finish-event)))

    (test-case "should compare elements using given key function"
      (define trans (deduplicating-consecutive #:key immutable-string-foldcase))
      (define inputs (list "foo" "FOO" "BAR" "bar" "Foo"))
      (define expected (list "foo" "BAR" "Foo"))
      (check-equal? (transduce inputs trans #:into into-list) expected)
      (check-equal? (transduce inputs (materializing trans) #:into into-list)
                    (list start-event
                          (consume-event "foo")
                          (emit-event "foo")
                          (consume-event "FOO")
                          (consume-event "BAR")
                          (emit-event "BAR")
                          (consume-event "bar")
                          (consume-event "Foo")
                          (emit-event "Foo")
                          half-close-event
                          finish-event)))))
