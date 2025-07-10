#lang racket/base


(require (submod rebellion/streaming/reducer2/base no-contract))


(provide into-binary-fold)


(define (into-binary-fold f x
                          #:finisher [finisher values]
                          #:ordered? [ordered? #true]
                          #:termination-checker [termination-checker #false])
  (make-reducer #:starter (λ () x)
                #:accumulator (λ (s e) (f e s))
                #:finisher finisher
                #:termination-checker termination-checker
                #:merger f
                #:cloner values
                #:ordered? ordered?
                #:concurrent? #true))
