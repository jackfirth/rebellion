#lang racket/base


(module+ test
  (require racket/list
           racket/set
           rackunit
           rebellion/collection/list
           rebellion/collection/set
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case (name-string shuffling)

    (test-case "should reuse single instance for default behavior"
      (check-eq? (shuffling) (shuffling)))

    (test-case "should do nothing on empty input"
      (check-equal? (transduce "" (shuffling) #:into into-string) "")
      (define actual-events
        (transduce ""
                   (observing-transduction-events (shuffling))
                   #:into into-list))
      (define expected-events (list start-event half-close-event finish-event))
      (check-equal? actual-events expected-events))

    (define input (list 'my 'quick 'brown 'fox 'jumped 'over 'the 'lazy 'dog))

    (test-case "should reorder elements"
      ;; Technically the shuffle has a miniscule chance of returning the elements in the same order
      ;; but we'll ignore that because it's almost impossible.
      (check-not-equal? (transduce input (shuffling) #:into into-list) input))

    (test-case "should not remove or add elements"
      (check-equal? (transduce input (shuffling) #:into into-set) (list->set input)))

    (test-case "should be nondeterministic"
      ;; This is also technically a flaky test, but only with astronomical odds.
      (check-not-equal? (transduce input (shuffling) #:into into-list)
                        (transduce input (shuffling) #:into into-list)))

    (test-case "should be deterministic with respect to the current RNG state"
      (define rng-state (pseudo-random-generator->vector (current-pseudo-random-generator)))
      (define first-shuffle (transduce input (shuffling) #:into into-list))
      (define second-shuffle
        (parameterize ([current-pseudo-random-generator (vector->pseudo-random-generator rng-state)])
          (transduce input (shuffling) #:into into-list)))
      (check-equal? first-shuffle second-shuffle))))
