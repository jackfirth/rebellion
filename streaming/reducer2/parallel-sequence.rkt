#lang racket/base


(require racket/contract/base)


(provide
 gen:parallel-sequence
 (contract-out
  [parallel-sequence? (-> any/c boolean?)]
  [parallel-sequence-spliterator (-> parallel-sequence? spliterator?)]))


(module+ unchecked
  (provide gen:parallel-sequence
           parallel-sequence?
           parallel-sequence-spliterator))


(require racket/generic
         rebellion/streaming/reducer2/spliterator
         rebellion/streaming/reducer2/vector-spliterator)


;@----------------------------------------------------------------------------------------------------


(define-generics parallel-sequence
  (parallel-sequence-spliterator parallel-sequence)
  #:requires [parallel-sequence-spliterator]

  #:fast-defaults
  ([vector?
    (define (parallel-sequence-spliterator this)
      (vector-spliterator this 0 (vector-length this)))]))
