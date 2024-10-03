#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [interned-symbol? (-> any/c boolean?)]
  [uninterned-symbol? (-> any/c boolean?)]
  [unreadable-symbol? (-> any/c boolean?)]))

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (interned-symbol? v) (and (symbol? v) (symbol-interned? v)))
(define (unreadable-symbol? v) (and (symbol? v) (symbol-unreadable? v)))

(define (uninterned-symbol? v)
  (and (symbol? v)
       (not (symbol-interned? v))
       (not (symbol-unreadable? v))))

(module+ test
  (test-case "symbol-predicates"
    (define x 'interned)
    (define y (string->unreadable-symbol "unreadable"))
    (define z (string->uninterned-symbol "uninterned"))
    (test-case "interned-symbol?"
      (check-true (interned-symbol? x))
      (check-false (interned-symbol? y))
      (check-false (interned-symbol? z))
      (check-false (interned-symbol? 42)))
    (test-case "unreadable-symbol?"
      (check-false (unreadable-symbol? x))
      (check-true (unreadable-symbol? y))
      (check-false (unreadable-symbol? z))
      (check-false (unreadable-symbol? 42)))
    (test-case "uninterned-symbol?"
      (check-false (uninterned-symbol? x))
      (check-false (uninterned-symbol? y))
      (check-true (uninterned-symbol? z))
      (check-false (uninterned-symbol? 42)))))
