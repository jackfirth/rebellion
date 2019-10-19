#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [impossible (-> none/c any/c)]))

;@------------------------------------------------------------------------------

(struct exn:fail:impossible exn:fail () #:transparent)

(define (impossible _)
  (raise (exn:fail:impossible "This should be impossible"
                              (current-continuation-marks))))
