#lang rebellion/private/dependencies/layer1

(provide
 (contract-out
  [natural? predicate?]))

(require rebellion/name
         rebellion/predicate
         (only-in racket/base exact-nonnegative-integer?))

;@------------------------------------------------------------------------------

(define natural?
  (make-predicate exact-nonnegative-integer? #:name (symbolic-name 'natural?)))
