#lang racket


(provide
 (contract-out
  #:unprotected-submodule no-contract
  [reducer? (-> any/c boolean?)]
  [make-reducer
   (->* (#:starter (-> any/c)
         #:accumulator (-> any/c any/c any/c))
        (#:finisher (-> any/c any/c)
         #:termination-checker (or/c (-> any/c boolean?) #false)
         #:cloner (or/c (-> any/c any/c) #false)
         #:merger (or/c (-> any/c any/c any/c) #false)
         #:ordered? boolean?
         #:concurrent? boolean?)
        reducer?)]
  [reducer-starter (-> reducer? (-> any/c))]
  [reducer-accumulator (-> reducer? (-> any/c any/c any/c))]
  [reducer-finisher (-> reducer? (-> any/c any/c))]
  [reducer-termination-checker (-> reducer? (or/c (-> any/c boolean?) #false))]
  [reducer-cloner (-> reducer? (or/c (-> any/c any/c) #false))]
  [reducer-merger (-> reducer? (or/c (-> any/c any/c any/c) #false))]
  [reducer-ordered? (-> reducer? boolean?)]
  [reducer-concurrent? (-> reducer? boolean?)]))



(struct reducer
  (starter ; (-> S)
   accumulator ; (-> S E S)
   finisher ; (-> S R)
   termination-checker ; (or/c (-> S boolean?) #false)
   cloner ; (or/c (-> S S) #false)
   merger ; (or/c (-> S S S) #false)
   ordered? ; boolean?
   concurrent?)) ; boolean?


(define (make-reducer #:starter starter
                      #:accumulator accumulator
                      #:finisher [finisher values]
                      #:termination-checker [termination-checker #false]
                      #:cloner [cloner #false]
                      #:merger [merger #false]
                      #:ordered? [ordered? #true]
                      #:concurrent? [concurrent? #false])
  (reducer starter
           accumulator
           finisher
           termination-checker
           cloner
           merger
           ordered?
           concurrent?))
