#lang racket/base


(module+ test
  (require racket/sequence
           racket/stream
           rackunit
           rebellion/base/comparator
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing
           rebellion/type/record))


;@----------------------------------------------------------------------------------------------------

(module+ test
  (test-case (name-string transposing)

    (test-case "nonterminating column reducer"

      (test-case "no input"
        (define actual
          (transduce '()
                     (observing-transduction-events (transposing #:into into-list))
                     #:into into-list))
        (check-equal? actual (list start-event half-close-event finish-event)))

      (test-case "single empty list"
        (define actual
          (transduce (list '())
                     (observing-transduction-events (transposing #:into into-list))
                     #:into into-list))
        (check-equal? actual (list start-event (consume-event '()) finish-event)))

      (test-case "multiple empty lists"
        (define actual
          (transduce (list '() '() '())
                     (observing-transduction-events (transposing #:into into-list))
                     #:into into-list))
        (define expected
          (list start-event (consume-event '()) finish-event))
        (check-equal? actual expected))

      (test-case "infinite empty lists"
        (define actual
          (transduce (for/stream ([_ (in-naturals)])
                       '())
                     (observing-transduction-events (transposing #:into into-list))
                     #:into into-list))
        (define expected
          (list start-event (consume-event '()) finish-event))
        (check-equal? actual expected))

      (test-case "single list"
        (define actual
          (transduce (list (list 1 2 3))
                     (observing-transduction-events (transposing #:into into-list))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                half-close-event
                (half-closed-emit-event (list 1))
                (half-closed-emit-event (list 2))
                (half-closed-emit-event (list 3))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple singleton lists"
        (define actual
          (transduce (list (list 1) (list 2) (list 3))
                     (observing-transduction-events (transposing #:into into-list))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1))
                (consume-event (list 2))
                (consume-event (list 3))
                half-close-event
                (half-closed-emit-event (list 1 2 3))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists"
        (define actual
          (transduce (list (list 1 2 3) (list 4 5 6) (list 7 8 9))
                     (observing-transduction-events (transposing #:into into-list))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 4 5 6))
                (consume-event (list 7 8 9))
                half-close-event
                (half-closed-emit-event (list 1 4 7))
                (half-closed-emit-event (list 2 5 8))
                (half-closed-emit-event (list 3 6 9))
                finish-event))
        (check-equal? actual expected)))

    (test-case "terminating reducer"

      (define into-list-until-stop
        (into-transduced (taking-while (λ (x) (not (equal? x 'stop)))) #:into into-list))

      (test-case "no input"
        (define actual
          (transduce '()
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected (list start-event half-close-event finish-event))
        (check-equal? actual expected))

      (test-case "single empty list"
        (define actual
          (transduce (list '())
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected (list start-event (consume-event '()) finish-event))
        (check-equal? actual expected))

      (test-case "multiple empty lists"
        (define actual
          (transduce (list '() '() '())
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected (list start-event (consume-event '()) finish-event))
        (check-equal? actual expected))

      (test-case "infinite empty lists"
        (define actual
          (transduce (for/stream ([_ (in-naturals)])
                       '())
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected (list start-event (consume-event '()) finish-event))
        (check-equal? actual expected))

      (test-case "multiple singleton lists"
        (define actual
          (transduce (list (list 1) (list 2) (list 3))
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1))
                (consume-event (list 2))
                (consume-event (list 3))
                half-close-event
                (half-closed-emit-event (list 1 2 3))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple singleton lists with stop"
        (define actual
          (transduce (list (list 1) (list 2) (list 3) (list 'stop) (list 4))
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1))
                (consume-event (list 2))
                (consume-event (list 3))
                (consume-event (list 'stop))
                (half-closed-emit-event (list 1 2 3))
                finish-event))
        (check-equal? actual expected))

      (test-case "infinite singleton lists with stop"
        (define input
          (sequence-append
           (list (list 1) (list 2) (list 3) (list 'stop))
           (for/stream ([n (in-naturals 4)])
             (list n))))
        (define actual
          (transduce input
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1))
                (consume-event (list 2))
                (consume-event (list 3))
                (consume-event (list 'stop))
                (half-closed-emit-event (list 1 2 3))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists"
        (define actual
          (transduce (list (list 1 2 3) (list 4 5 6) (list 7 8 9))
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 4 5 6))
                (consume-event (list 7 8 9))
                half-close-event
                (half-closed-emit-event (list 1 4 7))
                (half-closed-emit-event (list 2 5 8))
                (half-closed-emit-event (list 3 6 9))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists with first column stopping"
        (define actual
          (transduce (list (list 1 2 3) (list 'stop 5 6) (list 7 8 9))
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 'stop 5 6))
                (emit-event (list 1))
                (consume-event (list 7 8 9))
                half-close-event
                (half-closed-emit-event (list 2 5 8))
                (half-closed-emit-event (list 3 6 9))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists with first column stopping with unordered columns"
        (define actual
          (transduce (list (list 1 2 3) (list 'stop 5 6) (list 7 8 9))
                     (observing-transduction-events
                      (transposing #:into into-list-until-stop #:ordered? #false))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 'stop 5 6))
                (emit-event (list 1))
                (consume-event (list 7 8 9))
                half-close-event
                (half-closed-emit-event (list 2 5 8))
                (half-closed-emit-event (list 3 6 9))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists with last column stopping"
        (define actual
          (transduce (list (list 1 2 3) (list 4 5 'stop) (list 7 8 9))
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 4 5 'stop))
                (consume-event (list 7 8 9))
                half-close-event
                (half-closed-emit-event (list 1 4 7))
                (half-closed-emit-event (list 2 5 8))
                (half-closed-emit-event (list 3))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists with last column stopping with unordered columns"
        (define actual
          (transduce (list (list 1 2 3) (list 4 5 'stop) (list 7 8 9))
                     (observing-transduction-events
                      (transposing #:into into-list-until-stop #:ordered? #false))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 4 5 'stop))
                (emit-event (list 3))
                (consume-event (list 7 8 9))
                half-close-event
                (half-closed-emit-event (list 1 4 7))
                (half-closed-emit-event (list 2 5 8))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists with all stopping simultaneously"
        (define actual
          (transduce (list (list 1 2 3) (list 'stop 'stop 'stop) (list 7 8 9))
                     (observing-transduction-events (transposing #:into into-list-until-stop))
                     #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 'stop 'stop 'stop))
                (half-closed-emit-event (list 1))
                (half-closed-emit-event (list 2))
                (half-closed-emit-event (list 3))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists with all stopping separately"
        (define actual
          (transduce
           (list (list 1 2 3) (list 4 'stop 6) (list 'stop 8 9) (list 0 0 'stop) (list 0 0 0))
           (observing-transduction-events (transposing #:into into-list-until-stop))
           #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 4 'stop 6))
                (consume-event (list 'stop 8 9))
                (emit-event (list 1 4))
                (emit-event (list 2))
                (consume-event (list 0 0 'stop))
                (half-closed-emit-event (list 3 6 9))
                finish-event))
        (check-equal? actual expected))

      (test-case "multiple lists with all stopping separately with unordered columns"
        (define actual
          (transduce
           (list (list 1 2 3) (list 4 'stop 6) (list 'stop 8 9) (list 0 0 'stop) (list 0 0 0))
           (observing-transduction-events (transposing #:into into-list-until-stop #:ordered? #false))
           #:into into-list))
        (define expected
          (list start-event
                (consume-event (list 1 2 3))
                (consume-event (list 4 'stop 6))
                (emit-event (list 2))
                (consume-event (list 'stop 8 9))
                (emit-event (list 1 4))
                (consume-event (list 0 0 'stop))
                (half-closed-emit-event (list 3 6 9))
                finish-event))
        (check-equal? actual expected)))))
