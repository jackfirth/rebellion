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
    (check-equal? (transduce (list "foo" "FOO" "BAR" "bar" "Foo")
                             (deduplicating #:key immutable-string-foldcase)
                             #:into into-list)
                  (list "foo" "BAR")))

  (test-case "deduplicating-consecutive"
    (check-equal? (transduce (list 2 2 2 2 3 7 7 7 3 3 1 1 7 7)
                             (deduplicating-consecutive)
                             #:into into-list)
                  (list 2 3 7 3 1 7))
    (check-equal?
     (transduce (list "foo" "FOO" "BAR" "bar" "Foo")
                (deduplicating-consecutive #:key immutable-string-foldcase)
                #:into into-list)
     (list "foo" "BAR" "Foo"))))
