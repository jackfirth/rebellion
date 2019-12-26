#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     rebellion/collection/immutable-vector
                     rebellion/collection/vector
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/vector
                   'rebellion/streaming/transducer)
    #:private (list 'racket/base)))

@title{Vectors}
@defmodule[rebellion/collection/vector]

@defproc[(into-vector [#:size size (or/c natural? +inf.0) +inf.0]) reducer?]{
 Constructs a @tech{reducer} that collects at most @racket[size] elements of a
 sequence into an immutable vector.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-naturals)
              (filtering odd?)
              #:into (into-vector #:size 5)))}

@defproc[(into-mutable-vector [#:size size (or/c natural? +inf.0) +inf.0])
         reducer?]{
 Constructs a @tech{reducer} that collects at most @racket[size] elements of a
 sequence into a mutable vector.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-naturals)
              (filtering even?)
              #:into (into-mutable-vector #:size 5)))}

@defproc[(sequence->vector [seq (sequence/c any/c)]) immutable-vector?]{
 Copies @racket[seq] into an immutable vector. If @racket[seq] is already an
 immutable vector, it is returned directly.

 @(examples
   #:eval (make-evaluator) #:once
   (sequence->vector (list 1 2 3))
   (sequence->vector (in-range 0 10)))}
