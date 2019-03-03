#lang scribble/manual

@(require (for-label (except-in racket/base pair?)
                     racket/contract/base
                     rebellion/pair)
          rebellion/private/scribble-evaluator-factory
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/pair)
    #:private (list 'racket/base)))

@title{Pairs}
@defmodule[rebellion/pair]

A @deftech{pair} is a container of two values. Pairs as defined by
@racketmodname[rebellion/pair] are distinct from the pair datatype defined by
@racketmodname[racket/base], and are meant to be a drop-in replacement for them.
Changes include:

@itemlist[
 @item{@racket[cons] is spelled @racket[pair]}
 @item{@racket[car] is spelled @racket[pair-first]}
 @item{@racket[cdr] is spelled @racket[pair-second]}
 @item{@racket[list?] does not imply @racket[pair?]}
 @item{@racket[(pair 1 2)] writes as @literal{(pair 1 2)} instead of
  @literal{(1 . 2)}}]

Note that @racketmodname[rebellion/pair] provides a @racket[pair?] predicate
that conflicts with the one provided by @racketmodname[racket/base].

@defproc[(pair? [v any/c]) boolean?]{
 A predicate for @tech{pairs}, as defined by @racketmodname[rebellion/pair].
 Mututally exclusive with @racket[list?].

 @(examples
   #:eval (make-evaluator) #:once
   (pair? (pair 1 2))
   (list? (pair 1 2))
   (require (only-in racket/base cons))
   (pair? (cons 1 2)))}

@defproc[(pair [first any/c] [second any/c]) pair?]{
 Constructs a @tech{pair}.}

@defproc[(pair-first [p pair?]) any/c]{
 Returns the first value of @racket[p].}

@defproc[(pair-second [p pair?]) any/c]{
 Returns the second value of @racket[p].}
