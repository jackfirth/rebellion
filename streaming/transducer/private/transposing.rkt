#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [transposing
   (->* (#:into reducer?) (#:ordered? boolean?) (transducer/c (sequence/c any/c) any/c))]))


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


(struct transposing-state
  (reduction-state-vector
   [first-consuming #:mutable]
   [first-finished-early #:mutable])
  #:transparent)


(define (transposing-state-find-first-consuming! state)
  (define states (transposing-state-reduction-state-vector state))
  (define state-count (vector-length states))
  (define scan-start (or (transposing-state-first-consuming state) 0))
  (define first-consuming
    (for/first ([s (in-vector states scan-start)]
                [i (in-naturals scan-start)]
                #:when (and (variant? s) (variant-tagged-as? s '#:consume)))
      i))
  (set-transposing-state-first-consuming! state first-consuming))


(define (transposing-state-find-first-finished-early! state)
  (define states (transposing-state-reduction-state-vector state))
  (define state-count (vector-length states))
  (define scan-start (or (transposing-state-first-finished-early state) 0))
  (define first-finished-early
    (for/first ([s (in-vector states scan-start)]
                [i (in-naturals scan-start)]
                #:when (and (variant? s) (variant-tagged-as? s '#:early-finish)))
      i))
  (set-transposing-state-first-finished-early! state first-finished-early))


(define (make-first-state seq starter)
  (define elems (sequence->vector seq))
  (define states
    (for/vector ([_ (in-vector elems)])
      (starter)))
  (define state (transposing-state states #false #false))
  (transposing-state-find-first-consuming! state)
  (transposing-state-find-first-finished-early! state)
  state)


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
     "later sequence" seq))
  (set-transposing-state-first-consuming! state #false)
  (set-transposing-state-first-finished-early! state #false)
  (transposing-state-find-first-consuming! state)
  (transposing-state-find-first-finished-early! state))


(define/guard (transposing-state-next-action-unordered state)
  (match-define (transposing-state _ first-consuming first-finished-early) state)
  (cond
    [(and first-consuming first-finished-early) (variant #:emit state)]
    [first-finished-early (variant #:half-closed-emit state)]
    [first-consuming (variant #:consume state)]
    [else (variant #:finish #false)]))


(define/guard (transposing-state-next-action-ordered state)
  (match-define (transposing-state _ first-consuming first-finished-early) state)
  (cond
    [(and first-finished-early first-consuming)
     (if (< first-finished-early first-consuming) (variant #:emit state) (variant #:consume state))]
    [first-finished-early (variant #:half-closed-emit state)]
    [first-consuming (variant #:consume state)]
    [else (variant #:finish #false)]))


(define (transposing-state-emit-unordered! state early-finisher)
  (match-define (transposing-state state-vec _ first-finished-early) state)
  (match-define (variant #:early-finish substate) (vector-ref state-vec first-finished-early))
  (define result (early-finisher substate))
  (vector-set! state-vec first-finished-early #false)
  (transposing-state-find-first-finished-early! state)
  (emission (transposing-state-next-action-unordered state) result))


(define (transposing-state-emit-ordered! state early-finisher)
  (match-define (transposing-state state-vec _ first-finished-early) state)
  (match-define (variant #:early-finish substate) (vector-ref state-vec first-finished-early))
  (define result (early-finisher substate))
  (vector-set! state-vec first-finished-early #false)
  (transposing-state-find-first-finished-early! state)
  (emission (transposing-state-next-action-ordered state) result))


(define (transposing-state-next-half-closed-action-unordered state)
  (match-define (transposing-state _ first-consuming #false) state)
  (if first-consuming
      (variant #:half-closed-emit state)
      (variant #:finish #false)))


(define (transposing-state-next-half-closed-action-ordered state)
  (match-define (transposing-state _ first-consuming first-finished-early) state)
  (if (or first-consuming first-finished-early)
      (variant #:half-closed-emit state)
      (variant #:finish #false)))


(define (transposing-state-half-closed-emit-unordered! state finisher early-finisher)
  (match state
    [(transposing-state state-vec first-consuming #false)
     (match-define (variant #:consume substate) (vector-ref state-vec first-consuming))
     (define result (finisher substate))
     (vector-set! state-vec first-consuming #false)
     (transposing-state-find-first-consuming! state)
     (half-closed-emission (transposing-state-next-half-closed-action-unordered state) result)]
    [(transposing-state state-vec #false first-finished-early)
     (match-define (variant #:early-finish substate) (vector-ref state-vec first-finished-early))
     (define result (early-finisher substate))
     (vector-set! state-vec first-finished-early #false)
     (transposing-state-find-first-finished-early! state)
     (half-closed-emission (transposing-state-next-half-closed-action-ordered state) result)]))


(define (transposing-state-half-closed-emit-ordered! state finisher early-finisher)
  (match-define (transposing-state state-vec first-consuming first-finished-early) state)
  (cond
    [(< (or first-consuming +inf.0) (or first-finished-early +inf.0))
     (match-define (variant #:consume substate) (vector-ref state-vec first-consuming))
     (define result (finisher substate))
     (vector-set! state-vec first-consuming #false)
     (transposing-state-find-first-consuming! state)
     (half-closed-emission (transposing-state-next-half-closed-action-ordered state) result)]
    [(< (or first-finished-early +inf.0) (or first-consuming +inf.0))
     (match-define (variant #:early-finish substate) (vector-ref state-vec first-finished-early))
     (define result (early-finisher substate))
     (vector-set! state-vec first-finished-early #false)
     (transposing-state-find-first-finished-early! state)
     (half-closed-emission (transposing-state-next-half-closed-action-ordered state) result)]
    [else
     (raise-arguments-error
      (name transposing-state-half-closed-emit-ordered!)
      "this combination of first-consuming and first-finished-early should be impossible"
      "first-consuming" first-consuming
      "first-finished-early" first-finished-early
      "state vector" state-vec)]))


(define (transposing #:into transposition-reducer #:ordered? [ordered? #true])
  (define transposition-starter (reducer-starter transposition-reducer))
  (define transposition-consumer (reducer-consumer transposition-reducer))
  (define transposition-early-finisher (reducer-early-finisher transposition-reducer))
  (define transposition-finisher (reducer-finisher transposition-reducer))

  (define (start)
    (variant #:consume #false))

  (define consume
    (if ordered?
        (λ (state seq)
          (let ([state (or state (make-first-state seq transposition-starter))])
            (transposing-state-consume-sequence! state seq transposition-consumer)
            (transposing-state-next-action-ordered state)))
        (λ (state seq)
          (let ([state (or state (make-first-state seq transposition-starter))])
            (transposing-state-consume-sequence! state seq transposition-consumer)
            (transposing-state-next-action-unordered state)))))

  (define emit
    (if ordered?
        (λ (state)
          (transposing-state-emit-ordered! state transposition-early-finisher))
        (λ (state)
          (transposing-state-emit-unordered! state transposition-early-finisher))))

  (define half-close
    (if ordered?
        (λ (state)
          (if state
              (transposing-state-next-half-closed-action-ordered state)
              (variant #:finish #false)))
        (λ (state)
          (if state
              (transposing-state-next-half-closed-action-unordered state)
              (variant #:finish #false)))))

  (define half-closed-emit
    (if ordered?
        (λ (state)
          (transposing-state-half-closed-emit-ordered!
           state transposition-finisher transposition-early-finisher))
        (λ (state)
          (transposing-state-half-closed-emit-unordered!
           state transposition-finisher transposition-early-finisher))))

  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter emit
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name (name transposing)))
