#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sequence-empty? (-> sequence? boolean?)]))


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(define (sequence-empty? sequence)
  (define-values (has-next? get-next!) (sequence-generate sequence))
  (not (has-next?)))


(module+ test
  (test-case (name-string sequence-empty?)
    (check-true (sequence-empty? '()))
    (check-false (sequence-empty? (list 1)))
    (check-true (sequence-empty? (vector)))
    (check-false (sequence-empty? (vector 1)))
    (check-true (sequence-empty? (in-range 0 0)))
    (check-false (sequence-empty? (in-range 0 1)))))
