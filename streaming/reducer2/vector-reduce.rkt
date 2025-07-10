#lang racket/base


(require guard
         racket/future
         racket/list
         racket/math
         racket/vector
         (submod rebellion/streaming/reducer2/base no-contract))


(provide vector-parallel-reduce)


(define/guard (vector-parallel-reduce vec red)
  (unless (reducer-merger red)
    (error "reducer doesn't support parallelism"))
  (when (reducer-termination-checker red)
    (error "short-circuiting support not yet implemented"))
  (unless (equal? (reducer-cloner red) values)
    (error "cloning not yet implemented"))

  (define starter (reducer-starter red))
  (define finisher (reducer-finisher red))
  (guard (not (vector-empty? vec)) #:else (finisher (starter)))

  (define accumulator (reducer-accumulator red))
  (define merger (reducer-merger red))
  (define chunk-size (exact-ceiling (/ (vector-length vec) (processor-count))))
  (define futures
    (for/list ([start (in-range 0 (vector-length vec) chunk-size)])
      (define end (min (+ start chunk-size) (vector-length vec)))
      (future
       (λ ()
         (for/fold ([state (starter)])
                   ([elem (in-vector vec start end)])
           (accumulator state elem))))))
  (for/fold ([merged-state (touch (first futures))]
             #:result (finisher merged-state))
            ([next-future (in-list (rest futures))])
    (merger merged-state (touch next-future))))
