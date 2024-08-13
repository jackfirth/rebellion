#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [batching (-> reducer? transducer?)]))

(require rebellion/base/variant
         guard
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer/base
         rebellion/type/singleton
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-singleton-type unstarted-batch-placeholder)
(define-tuple-type batch (state))

(define/name (batching batch-reducer)
  (define batch-starter (reducer-starter batch-reducer))
  (define batch-consumer (reducer-consumer batch-reducer))
  (define batch-finisher (reducer-finisher batch-reducer))
  (define batch-early-finisher (reducer-early-finisher batch-reducer))
  (define (start-new-batch)
    (define init-state (batch-starter))
    (unless (variant-tagged-as? init-state '#:consume)
      (raise-arguments-error enclosing-function-name
                             "batch reducer must consume at least one value"
                             "batch reducer" batch-reducer
                             "batch start state" init-state))
    (batch (variant-value init-state)))
  (define (start) (variant #:consume unstarted-batch-placeholder))
  (define (consume current-batch v)
    (define state
      (batch-state
       (if (unstarted-batch-placeholder? current-batch)
           (start-new-batch)
           current-batch)))
    (define next-state (batch-consumer state v))
    (if (variant-tagged-as? next-state '#:consume)
        (variant #:consume (batch (variant-value next-state)))
        (variant #:emit (batch-early-finisher (variant-value next-state)))))
  (define (emit batch-result)
    (emission (start) batch-result))
  (define/guard (half-close last-batch)
    (guard (not (unstarted-batch-placeholder? last-batch)) #:else
      (variant #:finish #f))
    (define last-batch-result (batch-finisher (batch-state last-batch)))
    (variant #:half-closed-emit last-batch-result))
  (define (half-closed-emit last-batch-result)
    (half-closed-emission (variant #:finish #f) last-batch-result))
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter emit
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name enclosing-function-name))
