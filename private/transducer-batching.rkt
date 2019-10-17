#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [batching (-> reducer? transducer?)]))

(require rebellion/base/variant
         rebellion/streaming/reducer
         rebellion/streaming/transducer/base)

;@------------------------------------------------------------------------------

(define (batching batch-reducer)
  (define batch-starter (reducer-starter batch-reducer))
  (define batch-consumer (reducer-consumer batch-reducer))
  (define batch-finisher (reducer-finisher batch-reducer))
  (define batch-early-finisher (reducer-early-finisher batch-reducer))
  (define (start-batch)
    (define init-state (batch-starter))
    (unless (variant-tagged-as? init-state '#:consume)
      (raise-arguments-error 'batching
                             "batch reducer must consume at least one value"
                             "batch reducer" batch-reducer
                             "batch start state" init-state))
    init-state)
  (define (consume batch-state v)
    (define next-state (batch-consumer batch-state v))
    (if (variant-tagged-as? next-state '#:consume)
        next-state
        (variant #:emit (batch-early-finisher (variant-value next-state)))))
  (define (emit batch-result)
    (emission (start-batch) batch-result))
  (define (half-close last-batch-state)
    (define last-batch-result (batch-finisher last-batch-state))
    (variant #:half-closed-emit last-batch-result))
  (define (half-closed-emit last-batch-result)
    (half-closed-emission (variant #:finish #f) last-batch-result))
  (make-transducer
   #:starter start-batch
   #:consumer consume
   #:emitter emit
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name 'batching))
