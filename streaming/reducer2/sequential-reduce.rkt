#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  #:unprotected-submodule unchecked
  [sequential-reduce (-> parallel-sequence? reducer? any/c)]))


(require guard
         racket/math
         racket/future
         (submod rebellion/streaming/reducer2/base no-contract)
         (submod rebellion/streaming/reducer2/parallel-sequence unchecked)
         (submod rebellion/streaming/reducer2/spliterator unchecked))


;@----------------------------------------------------------------------------------------------------


(define/guard (sequential-reduce pseq red)
  (unless (equal? (reducer-cloner red) values)
    (error "cloning not yet implemented"))
  (define starter (reducer-starter red))
  (define finisher (reducer-finisher red))
  (define accumulator (reducer-accumulator red))
  (define terminate? (reducer-termination-checker red))
  (define init (starter))
  (guard (or (not terminate?) (not (terminate? init))) #:else (finisher init))
  (define split (parallel-sequence-spliterator pseq))
  (define final-state
    (cond
      [terminate?
       (define (accumulate-and-check state elem)
         (define next (accumulator state elem))
         (values next (not (terminate? next))))
       (spliterator-fold-with-break split accumulate-and-check init)]
      [else
       (spliterator-fold split accumulator init)]))
  (finisher final-state))
