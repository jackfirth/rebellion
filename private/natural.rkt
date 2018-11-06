#lang rebellion/private/dependencies/layer1

(provide
 (contract-out
  [natural? predicate?]))

(require rebellion/private/name-lite
         rebellion/private/predicate-lite
         (only-in racket/base exact-nonnegative-integer?))

;@------------------------------------------------------------------------------

(define natural?
  (make-predicate exact-nonnegative-integer? #:name (symbolic-name 'natural?)))
