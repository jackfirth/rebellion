#lang racket/base


(module+ test
  (require rackunit
           rebellion/base/immutable-string
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer))


;@------------------------------------------------------------------------------


(module+ test

  (test-case "taking-local-maxima"

    (test-case "empty sequence"
      (define actual (transduce '() (taking-local-maxima) #:into into-list))
      (check-equal? actual '()))

    (test-case "singleton sequence"
      (define actual (transduce (list 1) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "two-element ascending sequence"
      (define actual (transduce (list 1 2) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 2)))

    (test-case "two-element descending sequence"
      (define actual (transduce (list 2 1) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 2)))

    (test-case "three-element ascending sequence"
      (define actual (transduce (list 1 2 3) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 3)))

    (test-case "three-element descending sequence"
      (define actual (transduce (list 3 2 1) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 3)))

    (test-case "three-element peak sequence"
      (define actual (transduce (list 1 3 2) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 3)))

    (test-case "three-element valley sequence"
      (define actual (transduce (list 3 1 2) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 3 2)))

    (test-case "long alternating sequence"
      (define actual (transduce (list 1 -1 2 -2 3 -3) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 1 2 3)))

    (test-case "long ascending sequence"
      (define actual (transduce (list 1 2 3 4 5) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 5)))

    (test-case "long descending sequence"
      (define actual (transduce (list 5 4 3 2 1) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 5)))

    (test-case "sequence with plateau of maxima"
      (define actual (transduce (list 1 2 3 3 2 1) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 3)))

    (test-case "sequence with plateau of minima"
      (define actual (transduce (list 3 2 1 1 2 3) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 3 3)))

    (test-case "ascending sequence with plateau"
      (define actual (transduce (list 1 2 3 3 4 5) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 5)))

    (test-case "descending sequence with plateau"
      (define actual (transduce (list 5 4 3 3 2 1) (taking-local-maxima) #:into into-list))
      (check-equal? actual (list 5))))


  (test-case "taking-local-minima"

    (test-case "empty sequence"
      (define actual (transduce '() (taking-local-minima) #:into into-list))
      (check-equal? actual '()))

    (test-case "singleton sequence"
      (define actual (transduce (list 1) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "two-element ascending sequence"
      (define actual (transduce (list 1 2) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "two-element descending sequence"
      (define actual (transduce (list 2 1) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "three-element ascending sequence"
      (define actual (transduce (list 1 2 3) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "three-element descending sequence"
      (define actual (transduce (list 3 2 1) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "three-element peak sequence"
      (define actual (transduce (list 1 3 2) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1 2)))

    (test-case "three-element valley sequence"
      (define actual (transduce (list 3 1 2) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "long alternating sequence"
      (define actual (transduce (list 1 -1 2 -2 3 -3) (taking-local-minima) #:into into-list))
      (check-equal? actual (list -1 -2 -3)))

    (test-case "long ascending sequence"
      (define actual (transduce (list 1 2 3 4 5) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "long descending sequence"
      (define actual (transduce (list 5 4 3 2 1) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "sequence with plateau of maxima"
      (define actual (transduce (list 1 2 3 3 2 1) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1 1)))

    (test-case "sequence with plateau of minima"
      (define actual (transduce (list 3 2 1 1 2 3) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "ascending sequence with plateau"
      (define actual (transduce (list 1 2 3 3 4 5) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))

    (test-case "descending sequence with plateau"
      (define actual (transduce (list 5 4 3 3 2 1) (taking-local-minima) #:into into-list))
      (check-equal? actual (list 1)))))