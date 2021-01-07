#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/point)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/point)
    #:private (list 'racket/base)))

@title{Points}
@defmodule[rebellion/point]

A @deftech{point} is a location in two-dimensional Euclidean geometry,
represented by a pair of numbers.

@defproc[(point? [v any/c]) boolean?]{
 A predicate for @tech{points}.}

@defproc[(point [x real?] [y real?]) point?]{
 Constructs a @tech{point}.}

@deftogether[[
 @defproc[(point-x [pt point?]) real?]
 @defproc[(point-y [pt point?]) real?]]]{
 Accessors for the X and Y components of a @tech{point}.}
