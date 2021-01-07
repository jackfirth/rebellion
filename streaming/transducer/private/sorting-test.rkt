#lang racket/base

(module+ test
  (require rackunit
           rebellion/base/comparator
           rebellion/collection/list
           rebellion/streaming/transducer
           rebellion/type/record))

;@------------------------------------------------------------------------------

(module+ test
  (define-record-type gemstone (kind weight))
  (define (ruby weight) (gemstone #:kind 'ruby #:weight weight))
  (define (sapphire weight) (gemstone #:kind 'sapphire #:weight weight))
  (define (emerald weight) (gemstone #:kind 'emerald #:weight weight))
  (define (topaz weight) (gemstone #:kind 'topaz #:weight weight))
  
  (test-case "sorting"
    (test-case "empty-sequence"
      (check-equal? (transduce empty-list (sorting) #:into into-list)
                    empty-list))

    (test-case "one-element"
      (check-equal? (transduce (list 1) (sorting) #:into into-list) (list 1))
      (check-equal? (transduce (list (ruby 1))
                               (sorting #:key gemstone-weight)
                               #:into into-list)
                    (list (ruby 1)))
      (check-equal? (transduce (list "aaa")
                               (sorting string<=>)
                               #:into into-list)
                    (list "aaa")))

    (test-case "only-equivalent-elements"
      (check-equal? (transduce (list 1 1 1 1 1) (sorting) #:into into-list)
                    (list 1 1 1 1 1))
      (check-equal? (transduce (list (ruby 1) (sapphire 1) (emerald 1))
                               (sorting #:key gemstone-weight)
                               #:into into-list)
                    (list (ruby 1) (sapphire 1) (emerald 1)))
      (check-equal? (transduce (list "aaa" "aaa" "aaa" "aaa")
                               (sorting string<=>)
                               #:into into-list)
                    (list "aaa" "aaa" "aaa" "aaa")))

    (test-case "only-increasing-elements"
      (check-equal? (transduce (list 1 2 3 4 5) (sorting) #:into into-list)
                    (list 1 2 3 4 5))
      (check-equal? (transduce (list (ruby 1) (sapphire 2) (emerald 3))
                               (sorting #:key gemstone-weight)
                               #:into into-list)
                    (list (ruby 1) (sapphire 2) (emerald 3)))
      (check-equal? (transduce (list "aaa" "bbb" "ccc")
                               (sorting string<=>)
                               #:into into-list)
                    (list "aaa" "bbb" "ccc")))

    (test-case "only-nondecreasing-elements"
      (check-equal? (transduce (list 1 1 2 2 3 3) (sorting) #:into into-list)
                    (list 1 1 2 2 3 3))
      (define gems (list (ruby 1) (sapphire 1) (emerald 2) (topaz 2)))
      (check-equal? (transduce gems
                               (sorting #:key gemstone-weight)
                               #:into into-list)
                    gems)
      (check-equal? (transduce (list "aaa" "aaa" "bbb" "bbb" "ccc")
                               (sorting string<=>)
                               #:into into-list)
                    (list "aaa" "aaa" "bbb" "bbb" "ccc")))

    (test-case "only-decreasing-elements"
      (check-equal? (transduce (list 3 2 1) (sorting) #:into into-list)
                    (list 1 2 3))
      (check-equal? (transduce (list (ruby 3) (sapphire 2) (emerald 1))
                               (sorting #:key gemstone-weight)
                               #:into into-list)
                    (list (emerald 1) (sapphire 2) (ruby 3)))
      (check-equal? (transduce (list "ccc" "bbb" "aaa")
                               (sorting string<=>)
                               #:into into-list)
                    (list "aaa" "bbb" "ccc")))

    (test-case "only-nonincreasing-elements"
      (check-equal? (transduce (list 3 3 2 2 1 1) (sorting) #:into into-list)
                    (list 1 1 2 2 3 3))
      (define gems (list (ruby 2) (sapphire 2) (emerald 1) (topaz 1)))
      (check-equal? (transduce gems
                               (sorting #:key gemstone-weight)
                               #:into into-list)
                    (list (emerald 1) (topaz 1) (ruby 2) (sapphire 2)))
      (check-equal? (transduce (list "ccc" "ccc" "bbb" "aaa" "aaa")
                               (sorting string<=>)
                               #:into into-list)
                    (list "aaa" "aaa" "bbb" "ccc" "ccc")))

    (test-case "nonequivalent-elements"
      (check-equal? (transduce (list 4 1 2 5 3) (sorting) #:into into-list)
                    (list 1 2 3 4 5))
      (check-equal? (transduce (list 24 8 33 17 4 61 55 49)
                               (sorting)
                               #:into into-list)
                    (list 4 8 17 24 33 49 55 61))
      (check-equal? (transduce (list (ruby 4)
                                     (emerald 1)
                                     (sapphire 2)
                                     (topaz 5)
                                     (ruby 3))
                               (sorting #:key gemstone-weight)
                               #:into into-list)
                    (list (emerald 1)
                          (sapphire 2)
                          (ruby 3)
                          (ruby 4)
                          (topaz 5)))
      (check-equal? (transduce (list "ccc" "ddd" "aaa" "bbb" "eee")
                               (sorting string<=>)
                               #:into into-list)
                    (list "aaa" "bbb" "ccc" "ddd" "eee")))

    (test-case "arbitrary-elements"
      (check-equal? (transduce (list 2 1 4 1 4 1 2 3 5 3)
                               (sorting)
                               #:into into-list)
                    (list 1 1 1 2 2 3 3 4 4 5)))

    (test-case "descending-order"
      (define gems (list (ruby 2) (emerald 2) (sapphire 1) (topaz 3) (ruby 3)))
      (define expected
        (list (topaz 3) (ruby 3) (ruby 2) (emerald 2) (sapphire 1)))
      (define actual
        (transduce gems
                   (sorting #:key gemstone-weight #:descending? #t)
                   #:into into-list))
      (check-equal? actual expected))))
