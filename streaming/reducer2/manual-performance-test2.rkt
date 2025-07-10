#lang racket/base


(require rebellion/streaming/reducer2/parallel-reduce
         rebellion/streaming/reducer2/into-sum)


(collect-garbage)
(collect-garbage)
(collect-garbage)


(let ([vec (build-vector 100000000 values)])
  (time (parallel-reduce vec into-sum)))


(collect-garbage)
(collect-garbage)
(collect-garbage)


(let ([vec (build-vector 100000000 values)])
  (time (for/sum ([x (in-vector vec)]) x)))
