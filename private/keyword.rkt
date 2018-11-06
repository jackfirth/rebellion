#lang rebellion/private/dependencies/layer1

(provide
 (contract-out
  [keyword? predicate?]))

(require rebellion/private/name-lite
         rebellion/private/predicate-lite
         (only-in racket/base [keyword? racket:keyword?]))

;@------------------------------------------------------------------------------

(define keyword?
  (make-predicate racket:keyword? #:name (symbolic-name 'keyword?)))
