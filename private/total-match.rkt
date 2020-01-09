#lang racket/base

(provide define/total-match
         λ/match)

(require (for-syntax racket/base)
         racket/match
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define/total-match (name:id parameter-pattern:expr ...) body:expr ...+)
  #:with (parameter ...) (generate-temporaries #'(parameter-pattern ...))
  (define (name parameter ...)
    (match-define parameter-pattern parameter) ...
    body ...))

(define-simple-macro
  (λ/match (parameter-pattern:expr ...) body:expr ...+)
  #:with (parameter ...) (generate-temporaries #'(parameter-pattern ...))
  (λ (parameter ...)
    (match-define parameter-pattern parameter) ...
    body ...))

(module+ test
  (test-case (name-string define/total-match)
    (define/total-match (cons-pair-swap (cons x y)) (cons y x))
    (check-equal? (cons-pair-swap (cons 1 2)) (cons 2 1)))

  (test-case (name-string λ/match)
    (define cons-pair-swap (λ/match ((cons x y)) (cons y x)))
    (check-equal? (cons-pair-swap (cons 1 2)) (cons 2 1))))
