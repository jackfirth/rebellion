#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [point (-> real? real? point?)]
  [point? (-> real? boolean?)]
  [point-x (-> point? real?)]
  [point-y (-> point? real?)]))

(require rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type point (x y))
