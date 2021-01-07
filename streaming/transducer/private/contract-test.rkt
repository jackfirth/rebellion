#lang racket/base

(module+ test
  (require (submod "..")
           racket/contract/base
           racket/contract/combinator
           racket/contract/region
           rackunit
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/transducer))

;@------------------------------------------------------------------------------

(module+ test
  (test-case (name-string transducer/c)
    (test-case "should enforce the domain contract on consumed elements"
      (define/contract transducer (transducer/c number? any/c) (mapping add1))
      (check-not-exn
       (λ () (transduce (list 1 2 3) transducer #:into into-list)))
      (define (bad) (transduce (list 1 2 'foo 3) transducer #:into into-list))
      (check-exn exn:fail:contract:blame? bad)
      (check-exn #rx"expected: number\\?" bad)
      (check-exn #rx"given: 'foo" bad)
      (check-exn #rx"an element consumed by" bad))

    (test-case "should enforce the range contract on emitted elements"
      (define/contract transducer (transducer/c any/c integer?) (mapping add1))
      (check-not-exn
       (λ () (transduce (list 1 2 3) transducer #:into into-list)))
      (define (bad) (transduce (list 1 2 3.5) transducer #:into into-list))
      (check-exn exn:fail:contract:blame? bad)
      (check-exn #rx"promised: integer\\?" bad)
      (check-exn #rx"produced: 4\\.5" bad)
      (check-exn #rx"an element emitted by" bad))))
