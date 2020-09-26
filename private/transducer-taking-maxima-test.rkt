#lang racket/base

(module+ test
  (require rackunit
           rebellion/base/comparator
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))

(module+ test
  (test-case (name-string taking-maxima)

    (test-case "should emit the largest element"
      (define input (list 2 4 7 2 4 3 1))
      (define expected (list 7))
      (define actual (transduce input (taking-maxima) #:into into-list))
      (check-equal? actual expected))

    (test-case "should emit all of the largest elements if there's a tie"
      (define input (list 2 4 7 2 4 7 3 7 1))
      (define expected (list 7 7 7))
      (define actual (transduce input (taking-maxima) #:into into-list))
      (check-equal? actual expected))

    (test-case "should preserve the iteration order of emitted maxima"
      (define input (list 2 4 7 2 4 7.0 3 1))
      (define expected (list 7 7.0))
      (define actual (transduce input (taking-maxima) #:into into-list))
      (check-equal? actual expected))

    (test-case "should emit nothing for empty sequence"
      (define actual (transduce empty-list (taking-maxima) #:into into-list))
      (check-equal? actual empty-list))

    (test-case "should allow specifying a comparison key function"
      (define input (list "red" "blue" "yellow" "green" "purple" "black"))
      (define expected (list "yellow" "purple"))
      (define actual
        (transduce input (taking-maxima #:key string-length) #:into into-list))
      (check-equal? actual expected))

    (test-case "should allow specifying a custom comparator"
      (define input (list "cat" "dog" "zebra" "giraffe" "zebra" "dog"))
      (define expected (list "zebra" "zebra"))
      (define actual
        (transduce input (taking-maxima string<=>) #:into into-list))
      (check-equal? actual expected))

    (test-case "should emit elements only after half closing"
      (define transducer (observing-transduction-events (taking-maxima)))
      (define input (list 3 5 2 5 1))
      (define expected
        (list start-event
              (consume-event 3)
              (consume-event 5)
              (consume-event 2)
              (consume-event 5)
              (consume-event 1)
              half-close-event
              (half-closed-emit-event 5)
              (half-closed-emit-event 5)
              finish-event))
      (define actual (transduce input transducer #:into into-list))
      (check-equal? actual expected)))

  (test-case (name-string taking-minima)

    (test-case "should emit the smallest element"
      (define input (list 2 4 2 1 2 4 3 7))
      (define expected (list 1))
      (define actual (transduce input (taking-minima) #:into into-list))
      (check-equal? actual expected))

    (test-case "should emit all of the smallest elements if there's a tie"
      (define input (list 2 4 1 7 2 1 4 1 7))
      (define expected (list 1 1 1))
      (define actual (transduce input (taking-minima) #:into into-list))
      (check-equal? actual expected))

    (test-case "should preserve the iteration order of emitted minima"
      (define input (list 2 1 3 3 4 1.0 5))
      (define expected (list 1 1.0))
      (define actual (transduce input (taking-minima) #:into into-list))
      (check-equal? actual expected))

    (test-case "should emit nothing for empty sequence"
      (define actual (transduce empty-list (taking-minima) #:into into-list))
      (check-equal? actual empty-list))

    (test-case "should allow specifying a comparison key function"
      (define input (list "red" "blue" "yellow" "green" "purple" "black"))
      (define expected (list "red"))
      (define actual
        (transduce input (taking-minima #:key string-length) #:into into-list))
      (check-equal? actual expected))

    (test-case "should allow specifying a custom comparator"
      (define input (list "cat" "aardvark" "dog" "aardvark" "giraffe"))
      (define expected (list "aardvark" "aardvark"))
      (define actual
        (transduce input (taking-minima string<=>) #:into into-list))
      (check-equal? actual expected))

    (test-case "should emit elements only after half closing"
      (define transducer (observing-transduction-events (taking-minima)))
      (define input (list 3 1 2 1 5))
      (define expected
        (list start-event
              (consume-event 3)
              (consume-event 1)
              (consume-event 2)
              (consume-event 1)
              (consume-event 5)
              half-close-event
              (half-closed-emit-event 1)
              (half-closed-emit-event 1)
              finish-event))
      (define actual (transduce input transducer #:into into-list))
      (check-equal? actual expected))))
