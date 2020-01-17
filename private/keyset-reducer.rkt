#lang racket/base

(require racket/contract/base)

(provide
 for/keyset
 for*/keyset
 (contract-out
  [into-keyset (reducer/c keyword? keyset?)]))

(require (for-syntax racket/base)
         rebellion/collection/keyset/low-dependency
         rebellion/collection/list
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define into-keyset
  (reducer-map into-reversed-list #:range list->keyset))

(define-syntaxes (for/keyset for*/keyset)
  (make-reducer-based-for-comprehensions #'into-keyset))

(module+ test
  (test-case "into-keyset"
    (define strings (list "banana" "grape" "apple" "orange"))
    (check-equal? (for/keyset ([str (in-list strings)]) (string->keyword str))
                  (keyset #:apple #:banana #:grape #:orange))))
