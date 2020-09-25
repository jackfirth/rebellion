#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [windowing (->* (exact-positive-integer?) (#:into reducer?) transducer?)]))

(require racket/match
         rebellion/base/impossible-function
         rebellion/base/variant
         rebellion/private/static-name
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer/base
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type window-state
  (substates next-finished-substate partial-windows-remaining))

(define (initial-window-state size)
  (window-state
   #:substates (make-vector size #false)
   #:next-finished-substate 0
   #:partial-windows-remaining size))

(define/name (windowing window-size #:into [window-reducer into-list])
  (define window-starter (reducer-starter window-reducer))
  (define window-consumer (reducer-consumer window-reducer))
  (define window-finisher (reducer-finisher window-reducer))
  (define window-early-finisher (reducer-early-finisher window-reducer))
  (define (start) (variant #:consume (initial-window-state window-size)))
  (define (consume state element)
    (match state
      [(window-state
        #:substates substates
        #:next-finished-substate next-finished
        #:partial-windows-remaining (? zero?))
       (for ([i (in-naturals)]
             [substate (in-vector substates)]
             #:when (variant-tagged-as? substate '#:consume))
         (define next-consumer-substate
           (window-consumer (variant-value substate) element))
         (vector-set! substates i next-consumer-substate))
       (variant #:emit state)]
      [(window-state
        #:substates substates
        #:partial-windows-remaining (? positive? remaining))
       (define next-substate-index (- window-size remaining))
       (define num-substates (add1 next-substate-index))
       (vector-set! substates next-substate-index (window-starter))
       (for ([i (in-range 0 num-substates)]
             [substate (in-vector substates)]
             #:when (variant-tagged-as? substate '#:consume))
         (define next-consumer-substate
           (window-consumer (variant-value substate) element))
         (vector-set! substates i next-consumer-substate))
       (define next-state
         (window-state
          #:substates substates
          #:next-finished-substate 0
          #:partial-windows-remaining (sub1 remaining)))
       (if (equal? remaining 1)
           (variant #:emit next-state)
           (variant #:consume next-state))]))
  (define (emit state)
    (define substates (window-state-substates state))
    (define finished (window-state-next-finished-substate state))
    (define emitted-value
      (match (vector-ref substates finished)
        [(variant #:early-finish early-finish-state)
         (window-early-finisher early-finish-state)]
        [(variant #:consume consume-state)
         (window-finisher consume-state)]))
    (vector-set! substates finished (window-starter))
    (define next-state
      (window-state
       #:substates substates
       #:next-finished-substate (modulo (add1 finished) window-size)
       #:partial-windows-remaining 0))
    (emission (variant #:consume next-state) emitted-value))
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter emit
   #:half-closer (λ (state) (variant #:finish #false))
   #:half-closed-emitter impossible
   #:finisher void
   #:name enclosing-function-name))
