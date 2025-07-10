#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  #:unprotected-submodule unchecked
  [into-any-match? (-> (-> any/c boolean?) reducer?)]))


(require (submod rebellion/streaming/reducer2/base no-contract)
         rebellion/streaming/reducer2/reducer-map
         rebellion/streaming/reducer2/into-binary-fold)


;@----------------------------------------------------------------------------------------------------


(define (into-any-match? predicate)
  (reducer-map (into-binary-fold (λ (a b) (or a b)) #false
                                 #:termination-checker values
                                 #:ordered? #false)
               #:domain predicate))
