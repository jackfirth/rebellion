#lang racket/base

(module+ test
  (require rackunit
           rebellion/base/option
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))

;@------------------------------------------------------------------------------

(module+ test
  (test-case (name-string windowing)
    
    (test-case "default window reducer"
      (define transducer (windowing 3))
      (define elements (list 1 2 3 4 5 6 7))
      (define expected
        (list
         (list 1 2 3) (list 2 3 4) (list 3 4 5) (list 4 5 6) (list 5 6 7)))
      (check-equal? (transduce elements transducer #:into into-list) expected))
    
    (test-case "default window reducer transduction events"
      (define transducer (observing-transduction-events (windowing 3)))
      (define elements (list 1 2 3 4 5 6 7))
      (define expected
        (list start-event
              (consume-event 1)
              (consume-event 2)
              (consume-event 3)
              (emit-event (list 1 2 3))
              (consume-event 4)
              (emit-event (list 2 3 4))
              (consume-event 5)
              (emit-event (list 3 4 5))
              (consume-event 6)
              (emit-event (list 4 5 6))
              (consume-event 7)
              (emit-event (list 5 6 7))
              half-close-event
              finish-event))
      (define actual (transduce elements transducer #:into into-list))
      (check-equal? actual expected))

    (test-case "custom window reducer"
      (define transducer (windowing 3 #:into into-sum))
      (define elements (list 1 2 3 4 5 6 7))
      (define expected (list 6 9 12 15 18))
      (check-equal? (transduce elements transducer #:into into-list) expected))

    (test-case "custom window reducer transduction events"
      (define transducer
        (observing-transduction-events (windowing 3 #:into into-sum)))
      (define elements (list 1 2 3 4 5 6 7))
      (define expected
        (list start-event
              (consume-event 1)
              (consume-event 2)
              (consume-event 3)
              (emit-event 6)
              (consume-event 4)
              (emit-event 9)
              (consume-event 5)
              (emit-event 12)
              (consume-event 6)
              (emit-event 15)
              (consume-event 7)
              (emit-event 18)
              half-close-event
              finish-event))
      (check-equal? (transduce elements transducer #:into into-list) expected))

    (test-case "early-finishing window reducer"
      (define transducer (windowing 3 #:into (into-index-of #\i)))
      (define elements "driven")
      (define expected
        (list (present 2)
              (present 1)
              (present 0)
              absent))
      (check-equal? (transduce elements transducer #:into into-list) expected))

    (test-case "early-finishing window reducer transduction events"
      (define transducer
        (observing-transduction-events
         (windowing 3 #:into (into-index-of #\i))))
      (define elements "driven")
      (define expected
        (list start-event
              (consume-event #\d)
              (consume-event #\r)
              (consume-event #\i)
              (emit-event (present 2))
              (consume-event #\v)
              (emit-event (present 1))
              (consume-event #\e)
              (emit-event (present 0))
              (consume-event #\n)
              (emit-event absent)
              half-close-event
              finish-event))
      (check-equal? (transduce elements transducer #:into into-list) expected))

    (test-case "window size equal to sequence size"
      (define transducer (windowing 5))
      (define elements (list 1 2 3 4 5))
      (define expected (list (list 1 2 3 4 5)))
      (check-equal? (transduce elements transducer #:into into-list) expected))

    (test-case "window size equal to sequence size transduction events"
      (define transducer (observing-transduction-events (windowing 5)))
      (define elements (list 1 2 3 4 5))
      (define expected
        (list start-event
              (consume-event 1)
              (consume-event 2)
              (consume-event 3)
              (consume-event 4)
              (consume-event 5)
              (emit-event (list 1 2 3 4 5))
              half-close-event
              finish-event))
      (check-equal? (transduce elements transducer #:into into-list) expected))

    (test-case "window size greater than sequence size"
      (define transducer (windowing 5))
      (define elements (list 1 2 3 4))
      (check-equal?
       (transduce elements transducer #:into into-list) empty-list))

    (test-case "window size greater than sequence size transduction events"
      (define transducer (observing-transduction-events (windowing 5)))
      (define elements (list 1 2 3 4))
      (define expected
        (list start-event
              (consume-event 1)
              (consume-event 2)
              (consume-event 3)
              (consume-event 4)
              half-close-event
              finish-event))
      (check-equal? (transduce elements transducer #:into into-list) expected))

    (test-case "window size greater than sequence size with short-circuiting"
      (define transducer
        (observing-transduction-events (windowing 5 #:into into-first)))
      (define elements (list 1 2 3 4))
      (define expected
        (list start-event
              (consume-event 1)
              (consume-event 2)
              (consume-event 3)
              (consume-event 4)
              half-close-event
              finish-event))
      (define actual (transduce elements transducer #:into into-list))
      (check-equal? actual expected))))
