#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/match
                     rebellion/base/symbol
                     rebellion/type/enum
                     rebellion/type/tuple)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'racket/match
                   'rebellion/type/enum
                   'rebellion/type/tuple)
    #:private (list 'racket/base)))

@title{Enum Types}
@defmodule[rebellion/type/enum]

An @deftech{enum type} is a simple kind of @tech{data type} made up of a small
fixed set of named values. Enum types are useful for representing groups of
related constants, such as primary colors and directions.

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define-enum-type direction (up down left right))
   (define-tuple-type point (x y))

   (define/contract (point-move pt dir amount)
     (-> point? direction? real? point?)
     (match-define (point x y) pt)
     (match dir
       [(== up) (point x (+ y amount))]
       [(== down) (point x (- y amount))]
       [(== left) (point (- x amount) y)]
       [(== right) (point (+ x amount) y)])))
  
  (point-move (point 2 2) up 5)
  (point-move (point 1 4) left 10)
  (eval:error (point-move (point 1 4) 'up 5)))

@defform[
 (define-enum-type id (case-id ...) enum-option ...)
 #:grammar ([enum-option
             (code:line #:predicate-name predicate-id)
             (code:line #:property-maker property-maker)])
 #:contracts ([property-maker
               (-> uninitialized-enum-descriptor?
                   (listof (cons/c struct-type-property? any/c)))])]{
 Creates an @tech{enum type} named @racket[id]. Each @racket[case-id] is bound
 to a constant, and @racket[predicate-id] is bound to a predicate that returns
 @racket[#t] when given any of the constants and returns @racket[#f] for all
 other values. If @racket[#:predicate-name] is not given, then @racket[
 predicate-id] defaults to @racket[id]@racketidfont{?}.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-enum-type suit (diamonds clubs hearts spades))
    (define-enum-type face (jack queen king))
    (define-tuple-type card (type suit))

    (define king-of-clubs (card king clubs)))

   (card-type king-of-clubs)
   (card-suit king-of-clubs)

   (eval:no-prompt
    (define/contract (card-value c)
      (-> card? (integer-in 1 13))
      (match (card-type c)
        [(? number? x) x]
        [(== jack) 11]
        [(== queen) 12]
        [(== king) 13])))

   (card-value king-of-clubs)
   (card-value (card 7 spades))
   (card-value (card jack hearts)))}
