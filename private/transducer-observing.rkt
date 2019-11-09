#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [observing-transduction-events (-> transducer? transducer?)]
  [transduction-event? predicate/c]
  [start-event transduction-event?]
  [half-close-event transduction-event?]
  [finish-event transduction-event?]
  [consume-event (-> any/c consume-event?)]
  [consume-event? predicate/c]
  [consume-event-value (-> consume-event? any/c)]
  [emit-event (-> any/c emit-event?)]
  [emit-event? predicate/c]
  [emit-event-value (-> emit-event? any/c)]
  [half-closed-emit-event (-> any/c half-closed-emit-event?)]
  [half-closed-emit-event? predicate/c]
  [half-closed-emit-event-value (-> half-closed-emit-event? any/c)]))

(require rebellion/base/variant
         rebellion/private/static-name
         rebellion/streaming/transducer/base
         rebellion/type/record
         rebellion/type/singleton
         rebellion/type/wrapper)

;@------------------------------------------------------------------------------

(define-singleton-type start-event)
(define-singleton-type half-close-event)
(define-singleton-type finish-event)
(define-wrapper-type consume-event)
(define-wrapper-type emit-event)
(define-wrapper-type half-closed-emit-event)

(define (transduction-event? v)
  (or (start-event? v)
      (half-close-event? v)
      (finish-event? v)
      (consume-event? v)
      (emit-event? v)
      (half-closed-emit-event? v)))

(define-record-type materialized-transduction-step (event original-state))

(define/name (observing-transduction-events original)

  (define original-starter (transducer-starter original))
  (define original-consumer (transducer-consumer original))
  (define original-emitter (transducer-emitter original))
  (define original-half-closer (transducer-half-closer original))
  (define original-half-closed-emitter
    (transducer-half-closed-emitter original))
  (define original-finisher (transducer-finisher original))
  
  (define (start)
    (define step
      (materialized-transduction-step #:event start-event
                                      #:original-state (original-starter)))
    (variant #:emit step))
  
  (define (consume original-state element)
    (define step
      (materialized-transduction-step
       #:event (consume-event element)
       #:original-state (original-consumer original-state element)))
    (variant #:emit step))
  
  (define (emit step)
    (define event (materialized-transduction-step-event step))
    (define original-state (materialized-transduction-step-original-state step))
    (define next-state
      (case (variant-tag original-state)
        [(#:consume) original-state]
        [(#:emit)
         (define em (original-emitter (variant-value original-state)))
         (define next-step
           (materialized-transduction-step
            #:event (emit-event (emission-value em))
            #:original-state (emission-state em)))
         (variant #:emit next-step)]
        [(#:half-closed-emit)
         (define em
           (original-half-closed-emitter (variant-value original-state)))
         (define next-step
           (materialized-transduction-step
            #:event (half-closed-emit-event (half-closed-emission-value em))
            #:original-state (half-closed-emission-state em)))
         (variant #:half-closed-emit next-step)]
        [else
         (original-finisher (variant-value original-state))
         (define next-step
           (materialized-transduction-step
            #:event finish-event
            #:original-state #f))
         (variant #:half-closed-emit next-step)]))
    (emission next-state event))
  
  (define (half-close original-state)
    (define step
      (materialized-transduction-step
       #:event half-close-event
       #:original-state (original-half-closer original-state)))
    (variant #:half-closed-emit step))
  
  (define (half-closed-emit step)
    (define event (materialized-transduction-step-event step))
    (define original-state (materialized-transduction-step-original-state step))
    (define next-state
      (cond
        [(variant? original-state)
         (case (variant-tag original-state)
           [(#:half-closed-emit)
            (define em
              (original-half-closed-emitter (variant-value original-state)))
            (define next-step
              (materialized-transduction-step
               #:event (half-closed-emit-event (half-closed-emission-value em))
               #:original-state (half-closed-emission-state em)))
            (variant #:half-closed-emit next-step)]
           [else
            (original-finisher (variant-value original-state))
            (define next-step
              (materialized-transduction-step
               #:event finish-event
               #:original-state #f))
            (variant #:half-closed-emit next-step)])]
        [else (variant #:finish #f)]))
    (half-closed-emission next-state event))
  
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter emit
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name enclosing-function-name))
