#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [transposing (-> #:into reducer? (transducer/c (sequence/c any/c) any/c))]))


(require racket/match
         racket/sequence
         rebellion/base/variant
         rebellion/collection/vector
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer/base
         rebellion/streaming/transducer/private/contract)


;@----------------------------------------------------------------------------------------------------


(struct transposing-state (reduction-state-vector [previous-emission #:mutable]))


(define (make-first-state seq starter)
  (define elems (sequence->vector seq))
  (define states
    (for/vector ([_ (in-vector elems)])
      (starter)))
  (transposing-state states #false))


(define (transposing-state-consume-sequence! state seq consumer)
  (define state-vec (transposing-state-reduction-state-vector state))
  (define max-length (vector-length state-vec))
  (define seq-length
    (for/fold ([seq-length 0])
              ([v seq]
               [i (in-naturals)])
      (unless (< i max-length)
        (raise-arguments-error
         (name transposing)
         "transposed sequences not all the same length\n later sequence longer than first sequence"
         "first sequence length" max-length
         "later sequence" seq))
      (match (vector-ref state-vec i)
        [(variant #:consume state)
         (vector-set! state-vec i (consumer state v))]
        [(or (variant #:early-finish _) #false)
         (void)])
      (add1 seq-length)))
  (unless (equal? seq-length max-length)
    (raise-arguments-error
     (name transposing)
     "transposed sequences not all the same length\n later sequence shorter than first sequence"
     "first sequence length" max-length
     "later sequence" seq)))


(define/guard (transposing-state-next-action state)
  (match-define (transposing-state state-vec previous-emission) state)
  (define scan-start (if previous-emission (add1 previous-emission) 0))
  (define state-count (vector-length state-vec))
  (let loop ([i scan-start])
    (guarded-block
      (guard (equal? i state-count) then
        (set-transposing-state-previous-emission! state #false)
        (variant #:consume state))
      (match (vector-ref state-vec i)
        [(variant #:early-finish _)
         (set-transposing-state-previous-emission! state i)
         (variant #:emit state)]
        [(or #false (variant #:consume _))
         (loop (add1 i))]))))


(define (transposing-state-emit! state early-finisher)
  (match-define (transposing-state state-vec previous-emission) state)
  (match-define (variant #:early-finish substate) (vector-ref state-vec previous-emission))
  (define result (early-finisher substate))
  (vector-set! state-vec previous-emission #false)
  (emission (transposing-state-next-action state) result))


(define (transposing-state-next-half-closed-action state)
  (match-define (transposing-state state-vec previous-emission) state)
  (define scan-start (if previous-emission (add1 previous-emission) 0))
  (define state-count (vector-length state-vec))
  (let loop ([i scan-start])
    (guarded-block
      (guard (equal? i state-count) then
        (variant #:finish #false))
      (match (vector-ref state-vec i)
        [(variant #:consume _)
         (set-transposing-state-previous-emission! state i)
         (variant #:half-closed-emit state)]
        [#false (loop (add1 i))]))))


(define (transposing-state-half-closed-emit! state finisher)
  (match-define (transposing-state state-vec previous-emission) state)
  (match-define (variant #:consume substate) (vector-ref state-vec previous-emission))
  (define result (finisher substate))
  (vector-set! state-vec previous-emission #false)
  (half-closed-emission (transposing-state-next-half-closed-action state) result))


(define (transposing #:into transposition-reducer)
  (define transposition-starter (reducer-starter transposition-reducer))
  (define transposition-consumer (reducer-consumer transposition-reducer))
  (define transposition-early-finisher (reducer-early-finisher transposition-reducer))
  (define transposition-finisher (reducer-finisher transposition-reducer))

  (define (start)
    (variant #:consume #false))

  (define (consume state seq)
    (let ([state (or state (make-first-state seq transposition-starter))])
      (transposing-state-consume-sequence! state seq transposition-consumer)
      (transposing-state-next-action state)))

  (define (emit state)
    (transposing-state-emit! state transposition-early-finisher))

  (define (half-close state)
    (transposing-state-next-half-closed-action state))

  (define (half-closed-emit state)
    (transposing-state-half-closed-emit! state transposition-finisher))

  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter emit
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name (name transposing)))
