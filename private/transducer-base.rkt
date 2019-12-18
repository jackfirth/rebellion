#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [transduction-state/c flat-contract?]
  [half-closed-transduction-state/c flat-contract?]
  [transducer? predicate/c]
  [transducer-starter (-> transducer? any)]
  [transducer-consumer (-> transducer? any)]
  [transducer-emitter (-> transducer? any)]
  [transducer-half-closer (-> transducer? any)]
  [transducer-half-closed-emitter (-> transducer? any)]
  [transducer-finisher (-> transducer? any)]
  [emission (-> transduction-state/c any/c emission?)]
  [emission? predicate/c]
  [emission-state (-> emission? transduction-state/c)]
  [emission-value (-> emission? any/c)]
  [half-closed-emission
   (-> half-closed-transduction-state/c any/c half-closed-emission?)]
  [half-closed-emission? predicate/c]
  [half-closed-emission-state
   (-> half-closed-emission? half-closed-transduction-state/c)]
  [half-closed-emission-value (-> half-closed-emission? any/c)]
  [make-transducer
   (->* (#:starter (-> transduction-state/c)
         #:consumer (-> any/c any/c transduction-state/c)
         #:emitter (-> any/c emission?)
         #:half-closer (-> any/c half-closed-transduction-state/c)
         #:half-closed-emitter (-> any/c half-closed-emission?)
         #:finisher (-> any/c void?))
        (#:name (or/c interned-symbol? #f))
        transducer?)]))

(require rebellion/base/symbol
         rebellion/base/variant
         rebellion/type/reference
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define transduction-state/c variant?)
(define half-closed-transduction-state/c variant?)

(define-tuple-type emission (state value))
(define-tuple-type half-closed-emission (state value))

(define-reference-type transducer
  (starter consumer emitter half-closer half-closed-emitter finisher))
