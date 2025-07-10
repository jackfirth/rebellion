#lang racket


(require rebellion/streaming/reducer2/base)


(define into-list
  (make-reducer #:starter (λ () '())
                #:accumulator (λ (s e) (cons e s))
                #:finisher reverse
                #:cloner values
                #:merger (λ (left right) (append right left))
                #:concurrent? #true))
