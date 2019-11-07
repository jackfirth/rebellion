#lang racket/base

(module+ test
  (require rackunit
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))

;@------------------------------------------------------------------------------

(module+ test
  (test-case (name-string batching)
    (test-case "should-emit-last-batch-on-half-close"
      (define trans (materializing (batching (reducer-limit into-list 4))))
      (check-equal? (transduce (in-range 10) trans #:into into-list)
                    (list start-event
                          (consume-event 0)
                          (consume-event 1)
                          (consume-event 2)
                          (consume-event 3)
                          (emit-event (list 0 1 2 3))
                          (consume-event 4)
                          (consume-event 5)
                          (consume-event 6)
                          (consume-event 7)
                          (emit-event (list 4 5 6 7))
                          (consume-event 8)
                          (consume-event 9)
                          half-close-event
                          (half-closed-emit-event (list 8 9))
                          finish-event)))

    (test-case "should-not-emit-empty-batches"
      (define trans (materializing (batching (reducer-limit into-list 5))))
      (check-equal? (transduce (in-range 10) trans #:into into-list)
                    (list start-event
                          (consume-event 0)
                          (consume-event 1)
                          (consume-event 2)
                          (consume-event 3)
                          (consume-event 4)
                          (emit-event (list 0 1 2 3 4))
                          (consume-event 5)
                          (consume-event 6)
                          (consume-event 7)
                          (consume-event 8)
                          (consume-event 9)
                          (emit-event (list 5 6 7 8 9))
                          half-close-event
                          finish-event)))

    (test-case "should-emit-one-batch-if-reducer-never-finishes"
      (define trans (materializing (batching into-list)))
      (check-equal? (transduce (in-range 10) trans #:into into-list)
                    (list start-event
                          (consume-event 0)
                          (consume-event 1)
                          (consume-event 2)
                          (consume-event 3)
                          (consume-event 4)
                          (consume-event 5)
                          (consume-event 6)
                          (consume-event 7)
                          (consume-event 8)
                          (consume-event 9)
                          half-close-event
                          (half-closed-emit-event (list 0 1 2 3 4 5 6 7 8 9))
                          finish-event)))

    (test-case "should-emit-nothing-if-upstream-empty"
      (define trans (materializing (batching into-list)))
      (check-equal? (transduce empty-list trans #:into into-list)
                    (list start-event
                          half-close-event
                          finish-event)))))
