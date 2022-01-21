#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [check-precondition (unconstrained-domain-> void?)]))


;@----------------------------------------------------------------------------------------------------


(define (check-precondition precondition source-name error-message-template . template-args)
  (unless precondition
    (apply raise-arguments-error source-name error-message-template template-args)))
