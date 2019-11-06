#lang racket/base

(module+ test
  (require (submod "..")
           rackunit
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/streaming/transducer/testing))

;@------------------------------------------------------------------------------

(module+ test
  (test-case (name-string materializing)
    (define negating (mapping -))
    (check-equal?
     (transduce (list 1 2 3) (materializing negating) #:into into-list)
     (list start-event
           (consume-event 1)
           (emit-event -1)
           (consume-event 2)
           (emit-event -2)
           (consume-event 3)
           (emit-event -3)
           half-close-event
           finish-event))

    (check-equal?
     (transduce (in-naturals 1) (materializing (taking 3)) #:into into-list)
     (list start-event
           (consume-event 1)
           (emit-event 1)
           (consume-event 2)
           (emit-event 2)
           (consume-event 3)
           (half-closed-emit-event 3)
           finish-event))

    (check-equal?
     (transduce (list 1 2 3 4 5) (materializing (dropping 3)) #:into into-list)
     (list start-event
           (consume-event 1)
           (consume-event 2)
           (consume-event 3)
           (consume-event 4)
           (emit-event 4)
           (consume-event 5)
           (emit-event 5)
           half-close-event
           finish-event))

    (check-equal?
     (transduce (list 1 2 3 4 5)
                (materializing (batching into-sum))
                #:into into-list)
     (list start-event
           (consume-event 1)
           (consume-event 2)
           (consume-event 3)
           (consume-event 4)
           (consume-event 5)
           half-close-event
           (half-closed-emit-event 15)
           finish-event))))
