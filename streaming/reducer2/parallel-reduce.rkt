#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  #:unprotected-submodule unchecked
  [parallel-reduce (-> parallel-sequence? reducer? any/c)]
  [parallel-reduce-with-short-circuiting (-> parallel-sequence? reducer? any/c)]))


(require guard
         racket/math
         racket/future
         (submod rebellion/streaming/reducer2/base no-contract)
         (submod rebellion/streaming/reducer2/parallel-sequence unchecked)
         (submod rebellion/streaming/reducer2/spliterator unchecked))


;@----------------------------------------------------------------------------------------------------


(define (parallel-reduce pseq red)
  (if (reducer-termination-checker red)
      (parallel-reduce-with-short-circuiting pseq red)
      (parallel-reduce-without-short-circuiting pseq red)))


(define (parallel-reduce-without-short-circuiting pseq red)
  (unless (reducer-merger red)
    (error "reducer doesn't support parallelism"))
  (unless (equal? (reducer-cloner red) values)
    (error "cloning not yet implemented"))

  (define starter (reducer-starter red))
  (define finisher (reducer-finisher red))
  (define accumulator (reducer-accumulator red))
  (define merger (reducer-merger red))
  (define split (parallel-sequence-spliterator pseq))
  (define estimate (spliterator-size-estimate split))
  (define target-chunk-size (exact-ceiling (/ estimate (processor-count))))
  (finisher
   (let fork-loop ([split split])
     (guarded-block
       (define estimate (spliterator-size-estimate split))
       (guard (and estimate (> estimate target-chunk-size)) #:else
         (spliterator-fold split accumulator (starter)))
       (define-values (new-split forked-split) (spliterator-split split))
       (guard forked-split #:else
         (spliterator-fold new-split accumulator (starter)))
       (define fork-future (future (λ () (fork-loop forked-split))))
       (merger (fork-loop new-split) (touch fork-future))))))


(define (parallel-reduce-with-short-circuiting pseq red)
  (unless (reducer-merger red)
    (error "reducer doesn't support parallelism"))
  (unless (equal? (reducer-cloner red) values)
    (error "cloning not yet implemented"))
  
  (define starter (reducer-starter red))
  (define finisher (reducer-finisher red))
  (define accumulator (reducer-accumulator red))
  (define terminated? (reducer-termination-checker red))
  (define merger (reducer-merger red))
  (define split (parallel-sequence-spliterator pseq))
  (define estimate (spliterator-size-estimate split))
  (define target-chunk-size (exact-ceiling (/ estimate (processor-count))))

  (define original-custodian (current-custodian))
  (define user-code-custodian (make-custodian))
  (define root-future-custodian (make-custodian))

  (define (consume-spliterator-under-correct-custodian split)
    (parameterize (#;[current-custodian user-code-custodian])
      (spliterator-fold-with-break split accumulate-and-check (starter))))

  (define (accumulate-and-check state elem)
    (define new-state (accumulator state elem))
    (values new-state (not (terminated? new-state))))

  (define (terminated-under-correct-custodian? state)
    (parameterize (#;[current-custodian original-custodian])
      (terminated? state)))

  (define (merge-under-correct-custodian state other-state)
    (parameterize (#;[current-custodian original-custodian])
      (merger state other-state)))

  (finisher
   (touch
    (parameterize (#;[current-custodian root-future-custodian])
      (let fork-loop ([split split])
        (guarded-block
          (define estimate (spliterator-size-estimate split))
          (guard (and estimate (> estimate target-chunk-size)) #:else
            (future (λ () (consume-spliterator-under-correct-custodian split))))
          (define-values (new-split forked-split) (spliterator-split split))
          (guard forked-split #:else
            (future (λ () (consume-spliterator-under-correct-custodian new-split))))
          (define fork-custodian (make-custodian))
          (define right-future
            (parameterize ([current-custodian fork-custodian])
              (fork-loop forked-split)))
          (define left-future (fork-loop new-split))
          (future
           (λ ()
             (define left-state (touch left-future))
             
             (cond
               [(terminated-under-correct-custodian? left-state)
                (custodian-shutdown-all fork-custodian)
                left-state]
               [else
                (merge-under-correct-custodian left-state (touch right-future))])))))))))
