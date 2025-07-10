#lang racket


(require (submod rebellion/streaming/reducer2/into-any-match unchecked)
         rebellion/streaming/reducer2/sequential-reduce
         rebellion/streaming/reducer2/parallel-reduce)







(define (expensive-fib x)
  (cond
    [(equal? x 1) 1]
    [(equal? x 2) 1]
    [else (+ (expensive-fib (- x 1)) (expensive-fib (- x 2)))]))


(let* ([size 20]
       [vec (build-vector size values)])
  (time (sequential-reduce vec (into-any-match? (λ (x) (or (> x (expensive-fib 40)) (> x 10))))))
  (time (parallel-reduce vec (into-any-match? (λ (x) (or (> x (expensive-fib 40)) (> x 10)))))))
