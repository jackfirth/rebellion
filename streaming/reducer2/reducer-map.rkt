#lang racket/base


(require guard
         (submod rebellion/streaming/reducer2/base no-contract))


(provide reducer-map)


(define/guard (reducer-map base-reducer #:domain [f values] #:range [g values])
  (guard (or (not (equal? f values)) (not (equal? g values))) #:else base-reducer)
  (define accumulator (reducer-accumulator base-reducer))
  (define finisher (reducer-finisher base-reducer))
  (define mapping-accumulator (if (equal? f values) accumulator (λ (s e) (accumulator s (f e)))))
  (define mapping-finisher
    (cond
      [(equal? g values) finisher]
      [(equal? finisher values) g]
      [else (λ (s) (g (finisher s)))]))
  (make-reducer #:starter (reducer-starter base-reducer)
                #:accumulator mapping-accumulator
                #:finisher mapping-finisher
                #:termination-checker (reducer-termination-checker base-reducer)
                #:cloner (reducer-cloner base-reducer)
                #:merger (reducer-merger base-reducer)
                #:ordered? (reducer-ordered? base-reducer)
                #:concurrent? (reducer-concurrent? base-reducer)))
