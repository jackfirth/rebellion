#lang rebellion/private/dependencies/layer1

(provide
 and
 or
 (contract-out
  [boolean? predicate?]
  [not (-> boolean? boolean?)]))

(require rebellion/name
         rebellion/predicate
         (only-in racket/base
                  and
                  or
                  not
                  [boolean? racket:boolean?]))

;@------------------------------------------------------------------------------

(define boolean?
  (make-predicate racket:boolean? #:name (symbolic-name 'boolean?)))
