#lang racket/base

(module+ test
  (require rackunit
           rebellion/streaming/reducer
           rebellion/streaming/transducer))

;@------------------------------------------------------------------------------

(module+ test
  (test-case "deduplicating"
    (check-equal? (transduce "hello world" (deduplicating) #:into into-string)
                  "helo wrd")
    (check-equal? (transduce "zzzzzzzzzzz" (deduplicating) #:into into-string)
                  "z")))
