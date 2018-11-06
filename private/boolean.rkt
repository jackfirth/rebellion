#lang rebellion/private/dependencies/layer1

(provide
 (contract-out
  [boolean? predicate?]))

(require rebellion/private/name-lite
         rebellion/private/predicate-lite
         (only-in racket/base [boolean? racket:boolean?]))

;@------------------------------------------------------------------------------

(define boolean?
  (make-predicate racket:boolean? #:name (symbolic-name 'boolean?)))
