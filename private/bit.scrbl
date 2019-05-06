#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/bit)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/bit)
    #:private (list 'racket/base)))

@title{Bits}
@defmodule[rebellion/bit]

A @deftech{bit} is either zero or one. Eight bits form a @tech{byte}, and a
@tech{bitstring} is an arbitrary-length sequence of bits.

@defproc[(bit? [v any/c]) boolean?]{
 A predicate for @tech{bits}.}

@defproc[(bit->boolean [b bit?]) boolean?]{
 Converts zero to false and one to true.

 @(examples
   #:eval (make-evaluator) #:once
   (bit->boolean 0)
   (bit->boolean 1))}

@defproc[(boolean->bit [bool boolean?]) bit?]{
 Converts true to one and false to zero.

 @(examples
   #:eval (make-evaluator) #:once
   (boolean->bit #t)
   (boolean->bit #f))}
