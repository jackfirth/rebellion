#lang racket/base

(require racket/unsafe/ops
         rebellion/streaming/reducer2/into-sum
         rebellion/streaming/reducer2/vector-reduce)

(let ([vec (build-vector 100000000 values)])
  (time (vector-parallel-reduce vec into-sum)))
