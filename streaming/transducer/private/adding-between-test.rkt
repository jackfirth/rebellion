#lang racket/base


(module+ test
  (require rackunit
           rebellion/base/immutable-string
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case (name-string adding-between)

    (test-case "should do nothing for empty sequences"
      (define actual-events
        (transduce '() (observing-transduction-events (adding-between 42)) #:into into-list))
      
      (define expected-events
        (list start-event
              half-close-event
              finish-event))
      (check-equal? actual-events expected-events))

    (test-case "should leave singleton sequences unchanged"
      (define actual-events
        (transduce (list 'foo) (observing-transduction-events (adding-between 42)) #:into into-list))
      
      (define expected-events
        (list start-event
              (consume-event 'foo)
              half-close-event
              (half-closed-emit-event 'foo)
              finish-event))
      (check-equal? actual-events expected-events))

    (test-case "should add an element in the middle of two-element sequences"
      (define actual-events
        (transduce (list 'foo 'bar)
                   (observing-transduction-events (adding-between 42))
                   #:into into-list))
      
      (define expected-events
        (list start-event
              (consume-event 'foo)
              (consume-event 'bar)
              (emit-event 'foo)
              (emit-event 42)
              half-close-event
              (half-closed-emit-event 'bar)
              finish-event))
      (check-equal? actual-events expected-events))

    (test-case "should add an element between each element of a many-element sequence"
      (define actual-events
        (transduce (list 'a 'b 'c 'd 'e)
                   (observing-transduction-events (adding-between 42))
                   #:into into-list))
      
      (define expected-events
        (list start-event
              (consume-event 'a)
              (consume-event 'b)
              (emit-event 'a)
              (emit-event 42)
              (consume-event 'c)
              (emit-event 'b)
              (emit-event 42)
              (consume-event 'd)
              (emit-event 'c)
              (emit-event 42)
              (consume-event 'e)
              (emit-event 'd)
              (emit-event 42)
              half-close-event
              (half-closed-emit-event 'e)
              finish-event))
      (check-equal? actual-events expected-events))))
