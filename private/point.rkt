#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [point (-> real? real? point?)]
  [point? (-> real? boolean?)]
  [point-x (-> point? real?)]
  [point-y (-> point? real?)]))

(require rebellion/tuple-type)

;@------------------------------------------------------------------------------

(define point-descriptor (make-tuple-type (tuple-type 'point 2)))
(define point (tuple-descriptor-constructor point-descriptor))
(define point? (tuple-descriptor-predicate point-descriptor))
(define point-x (make-tuple-field-accessor point-descriptor 0 'x))
(define point-y (make-tuple-field-accessor point-descriptor 1 'y))
