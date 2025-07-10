#lang racket



(require rebellion/streaming/reducer2/parallel-reduce
         rebellion/streaming/reducer2/sequential-reduce
         (submod rebellion/streaming/reducer2/into-any-match unchecked))



(define size 1000000000)

(collect-garbage)
(collect-garbage)
(collect-garbage)

(let ([vec (build-vector size values)])
  (time (parallel-reduce vec (into-any-match? (λ (x) (> x (/ size 2)))))))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(let ([vec (build-vector size values)])
  (time (for/or ([x (in-vector vec)]) (> x (/ size 2)))))


(collect-garbage)
(collect-garbage)
(collect-garbage)

(let ([vec (build-vector size values)])
  (time (sequential-reduce vec (into-any-match? (λ (x) (> x (/ size 2)))))))
