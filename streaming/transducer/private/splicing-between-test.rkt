#lang racket/base


(module+ test
  (require rackunit
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case (name-string splicing-between)

    (test-case "should do nothing for empty sequences"
      (define actual-events
        (transduce "" (observing-transduction-events (splicing-between ", ")) #:into into-list))
      
      (define expected-events
        (list start-event
              half-close-event
              finish-event))
      (check-equal? actual-events expected-events))

    (test-case "should leave singleton sequences unchanged"
      (define actual-events
        (transduce "x" (observing-transduction-events (splicing-between ", ")) #:into into-list))
      
      (define expected-events
        (list start-event
              (consume-event #\x)
              half-close-event
              (half-closed-emit-event #\x)
              finish-event))
      (check-equal? actual-events expected-events))
    
    (test-case "should add an element in the middle of two-element sequences"
      (define actual-events
        (transduce "xy" (observing-transduction-events (splicing-between ", ")) #:into into-list))
      
      (define expected-events
        (list start-event
              (consume-event #\x)
              (consume-event #\y)
              (emit-event #\x)
              (emit-event #\,)
              (emit-event #\space)
              half-close-event
              (half-closed-emit-event #\y)
              finish-event))
      (check-equal? actual-events expected-events))

    (test-case "should add an element between each element of a many-element sequence"
      (define actual-events
        (transduce "wxyz"
                   (observing-transduction-events (splicing-between ", "))
                   #:into into-list))
      
      (define expected-events
        (list start-event
              (consume-event #\w)
              (consume-event #\x)
              (emit-event #\w)
              (emit-event #\,)
              (emit-event #\space)
              (consume-event #\y)
              (emit-event #\x)
              (emit-event #\,)
              (emit-event #\space)
              (consume-event #\z)
              (emit-event #\y)
              (emit-event #\,)
              (emit-event #\space)
              half-close-event
              (half-closed-emit-event #\z)
              finish-event))
      (check-equal? actual-events expected-events))

    (struct intiation-counting-sequence (base-sequence [count #:mutable])
      #:property prop:sequence
      (λ (this)
        (set-intiation-counting-sequence-count! this (add1 (intiation-counting-sequence-count this)))
        (intiation-counting-sequence-base-sequence this)))

    (define (sequence-count-initations seq)
      (intiation-counting-sequence seq 0))

    (test-case "should initiate the separator sequence each time it's inserted"
      (define separator-seq (sequence-count-initations ", "))
      (transduce "abcde" (splicing-between separator-seq) #:into into-string)
      (check-equal? (intiation-counting-sequence-count separator-seq) 4))

    (test-case "should support infinite separator sequences via lazy iteration"
      (define actual
        (transduce (list 'foo 'bar)
                   (splicing-between (in-naturals))
                   (taking 5)
                   #:into into-list))
      (check-equal? actual (list 'foo 0 1 2 3)))))
