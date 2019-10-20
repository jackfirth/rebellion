#lang racket/base

(module+ test
  (require rackunit
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
    (check-equal? (transduce empty-list (sorting) #:into into-list)
                  empty-list)
    (check-equal? (transduce (list 1) (sorting) #:into into-list)
                  (list 1))
    (check-equal? (transduce (list 1 1 1) (sorting) #:into into-list)
                  (list 1 1 1))
    (check-equal? (transduce (list (ruby 1) (sapphire 1) (emerald 1) (topaz 1))
                             (sorting #:key gemstone-weight)
                             #:into into-list)
                  (list (ruby 1) (sapphire 1) (emerald 1) (topaz 1)))))
