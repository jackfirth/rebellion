#lang racket/base


(require racket/unsafe/ops
         rebellion/streaming/reducer2/into-binary-fold)


(provide into-sum)


(define into-sum (into-binary-fold unsafe-fx+ 0 #:ordered? #false))
