#lang racket/base


(module+ test
  (require rackunit
           rebellion/base/option
           rebellion/private/static-name
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/type/tuple))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case (name-string reducer-zip)

    (test-case "no early finishing"
      (define into-average (reducer-zip / into-sum into-count))
      (check-equal? (transduce (list 1 2 3 4 5) #:into into-average) 3)
      (check-equal? (transduce (list 2) #:into into-average) 2))

    (test-case "one reducer finishes early"
      (define-tuple-type endpoints (first last))
      (define into-endpoints (reducer-zip endpoints nonempty-into-first nonempty-into-last))
      (check-equal? (transduce (list 1 2 3 4 5) #:into into-endpoints) (endpoints 1 5)))

    (test-case "all reducers finish early"
      (define-tuple-type first-two-elements (first second))
      (define into-first-two-elements (reducer-zip first-two-elements into-first (into-nth 1)))
      (define actual (transduce (in-naturals) #:into into-first-two-elements))
      (check-equal? actual (first-two-elements (present 0) (present 1))))))