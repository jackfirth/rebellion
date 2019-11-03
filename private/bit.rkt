#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [bit? predicate/c]
  [bit->boolean (-> bit? boolean?)]
  [boolean->bit (-> boolean? bit?)]))

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define (bit? v) (or (zero? v) (equal? v 1)))

(define (bit->boolean b) (equal? b 1))
(define (boolean->bit b) (if b 1 0))

(module+ test
  (test-case (name-string bit->boolean)
    (check-false (bit->boolean 0))
    (check-true (bit->boolean 1)))
  (test-case (name-string boolean->bit)
    (check-equal? (boolean->bit #f) 0)
    (check-equal? (boolean->bit #t) 1)))
