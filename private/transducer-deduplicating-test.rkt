#lang racket/base

(module+ test
  (require rackunit
           rebellion/base/immutable-string
           rebellion/collection/list
           rebellion/streaming/reducer
           rebellion/streaming/transducer))

;@------------------------------------------------------------------------------

(module+ test
  (test-case "deduplicating"
    (check-equal? (transduce "hello world" (deduplicating) #:into into-string)
                  "helo wrd")
    (check-equal? (transduce "zzzzzzzzzzz" (deduplicating) #:into into-string)
                  "z")
    (check-equal? (transduce (list "foo" "FOO" "BAR" "bar")
                             (deduplicating #:key immutable-string-foldcase)
                             #:into into-list)
                  (list "foo" "BAR"))))
