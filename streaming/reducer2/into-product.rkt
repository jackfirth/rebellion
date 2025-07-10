#lang racket/base


(require rebellion/streaming/reducer2/base
         rebellion/streaming/reducer2/into-binary-fold)


(provide into-product)


(define into-product (into-binary-fold * 1 #:ordered? #false))
