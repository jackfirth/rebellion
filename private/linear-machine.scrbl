#lang scribble/manual

@(require (for-label rebellion/linear-machine))

@title{Linear Machines}
@defmodule[rebellion/linear-machine]

A @deftech{linear machine} is a type of state machine.

@defproc[(linear-machine? [v any/c]) boolean?]{
 A predicate for @tech{linear machines}.}

@defproc[(linear-state? [v any/c]) boolean?]{
 A predicate for linear states.}

@defproc[(linear-machine-start [machine linear-machine?])
         (linear-state/c machine)]{
 Starts an instance of @racket[machine] and returns its initial state.}

@defproc[(fold-machine [f (-> any/c any/c any/c)] [init any/c])
         (linear-machine/c fold-states fold-consume fold-finish)]{
 Constructs a @deftech{linear machine} that performs a left fold over the
 values it consumes.}

@defproc[(linear-machine/c [states linear-state-set?]
                           [action (linear-action/c states)] ...)
         contract?]{
 A contract combinator for @tech{linear machines}.}
