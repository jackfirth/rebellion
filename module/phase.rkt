#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [phase (-> (or/c exact-integer? #f) phase?)]
  [phase? (-> any/c boolean?)]
  [phase-level (-> phase? (or/c exact-integer? #f))]
  [phase-shift (-> phase? exact-integer? phase?)]
  [label-phase label-phase?]
  [label-phase? (-> any/c boolean?)]
  [execution-phase (-> exact-integer? execution-phase?)]
  [execution-phase? (-> any/c boolean?)]
  [execution-phase-level (-> execution-phase? exact-integer?)]
  [execution-phase-shift (-> execution-phase? exact-integer? execution-phase?)]
  [runtime-phase execution-phase?]
  [compile-phase execution-phase?]
  [compile-phase-for (-> execution-phase? execution-phase?)]
  [template-phase execution-phase?]
  [template-phase-for (-> execution-phase? execution-phase?)]
  [meta-compile-phase execution-phase?]))

(require rebellion/type/singleton
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-tuple-type execution-phase (level))
(define-singleton-type label-phase)

(define (phase? v) (or (execution-phase? v) (label-phase? v)))
(define (phase level) (if level (execution-phase level) label-phase))

(define (phase-level ph)
  (and (not (label-phase? ph)) (execution-phase-level ph)))

(define runtime-phase (phase 0))
(define compile-phase (phase 1))
(define template-phase (phase -1))
(define meta-compile-phase (phase 2))

(module+ test
  (test-case "phase-smart-constructor"
    (check-pred execution-phase? runtime-phase)
    (check-pred execution-phase? compile-phase)
    (check-pred execution-phase? template-phase)
    (check-pred execution-phase? meta-compile-phase)
    (check-pred label-phase? (phase #f))))

(define (phase-shift ph relative-level)
  (if (label-phase? ph)
      label-phase
      (execution-phase-shift ph relative-level)))

(define (execution-phase-shift ph relative-level)
  (execution-phase (+ (execution-phase-level ph) relative-level)))

(define (compile-phase-for ph) (execution-phase-shift ph 1))
(define (template-phase-for ph) (execution-phase-shift ph -1))
