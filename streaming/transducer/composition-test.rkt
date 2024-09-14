#lang racket/base

(module+ test
  (require rackunit
           rebellion/base/immutable-string
           rebellion/base/impossible-function
           rebellion/base/variant
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))

(module+ test
  (test-case (name-string transducer-pipe)
    (test-case "zero transducers"
      (define piped (transducer-pipe))
      (check-equal? (transduce "hello" piped #:into into-string) "hello")
      (check-equal? (transduce (in-naturals) piped (taking 5) #:into into-list)
                    (list 0 1 2 3 4)))
    
    (test-case "one transducer"
      (define piped (transducer-pipe (taking 3)))
      (check-equal? (transduce "hello" piped #:into into-string) "hel")
      (check-equal? (transduce (in-naturals) piped #:into into-list)
                    (list 0 1 2))
      (check-equal? (transduce (list 0 1) piped #:into into-list) (list 0 1)))
    
    (test-case "two transducers"
      (define piped
        (transducer-pipe (filtering even?) (mapping number->immutable-string)))
      (check-equal?
       (transduce (in-range 0 10) piped #:into (join-into-string ","))
       "0,2,4,6,8"))

    (test-case "many transducers"
      (define piped
        (transducer-pipe (filtering even?)
                         (mapping (λ (x) (* x x)))
                         (taking 10)
                         (mapping -)
                         (append-mapping number->immutable-string)))
      (define inputs (in-naturals))
      (define expected "0-4-16-36-64-100-144-196-256-324")
      (check-equal? (transduce inputs piped #:into into-string) expected))

    (define (emitting value)
      (define emit-state (variant #:emit #false))
      (define em (emission emit-state value))
      (make-transducer
       #:starter (λ () emit-state)
       #:consumer (λ (state element) (error "impossible"))
       #:emitter (λ (_) em)
       #:half-closer (λ (_) (variant #:finish #false))
       #:half-closed-emitter impossible
       #:finisher void))

    (define (half-closed-emitting value)
      (define emit-state (variant #:half-closed-emit #false))
      (define em (half-closed-emission emit-state value))
      (make-transducer
       #:starter (λ () emit-state)
       #:consumer (λ (state element) (error "impossible"))
       #:emitter impossible
       #:half-closer impossible
       #:half-closed-emitter (λ (_) em)
       #:finisher void))

    (define finishing
      (make-transducer
       #:starter (λ () (variant #:finish #false))
       #:consumer (λ (state element) (error "impossible"))
       #:emitter impossible
       #:half-closer impossible
       #:half-closed-emitter impossible
       #:finisher void))

    (test-case "half closed upstream emisisons makes downstream half closed"
      (define piped
        (observing-transduction-events
         (transducer-pipe (half-closed-emitting #\a)
                          (taking 3))))
      (define inputs (in-naturals))
      (define expected
        (list start-event
              (half-closed-emit-event #\a)
              (half-closed-emit-event #\a)
              (half-closed-emit-event #\a)
              finish-event))
      (check-equal? (transduce inputs piped #:into into-list) expected))

    (test-case "finished upstream makes downstream half closed"
      (define piped
        (observing-transduction-events
         (transducer-pipe finishing (emitting #\a) (taking 3))))
      (define inputs (in-naturals))
      (define expected
        (list start-event
              (half-closed-emit-event #\a)
              (half-closed-emit-event #\a)
              (half-closed-emit-event #\a)
              finish-event))
      (check-equal? (transduce inputs piped #:into into-list) expected))))
