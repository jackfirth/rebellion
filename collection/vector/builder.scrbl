#lang scribble/manual

@(require (for-label (except-in racket/base build-vector)
                     racket/contract/base
                     racket/math
                     racket/sequence
                     rebellion/collection/immutable-vector
                     rebellion/collection/list
                     rebellion/collection/vector/builder)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/vector/builder)
    #:private (list 'racket/base)))

@title{Vector Builders}
@defmodule[rebellion/collection/vector/builder]

A @deftech{vector builder} is a single-use object for building a vector. First
create a builder with @racket[make-vector-builder], then repeatedly add elements
to it with @racket[vector-builder-add] or @racket[vector-builder-add-all], and
finally construct a vector using either @racket[build-vector] or @racket[
 build-mutable-vector].

Vector builders try to efficiently minimize allocation when adding more elements
to a builder. Constructing a vector of size N using a builder performs log(N)
allocations and consumes an amount of space linear in N.

Vector builders are @deftech{linear objects}. Each operation on a vector builder
destructively @emph{uses} the builder and returns an updated, unused builder.
Used builders cannot be reused; attempting to do so @emph{may} raise a contract
error, but this is not guaranteed.

@(examples
  #:eval (make-evaluator) #:once
  (define builder (make-vector-builder))
  (define builder2 (vector-builder-add builder 1 2 3))
  (eval:error (vector-builder-add builder 1 2 3)))

@defproc[(vector-builder? [v any/c]) boolean?]{
 A predicate for @tech{vector builders}.}

@defthing[unused-vector-builder/c flat-contract?]{
 A @tech/reference{flat contract} for @tech{vector builders} that are @emph{
  unused}. Passing a vector builder as an input to functions like @racket[
 vector-builder-add] and @racket[build-vector] @emph{uses} the builder, and the
 @racket[unused-vector-builder/c] contract can detect when a builder has already
 been used. Detection is not guaranteed, because those functions may or may not
 choose to mutate the original builder and reuse it instead of allocating a new
 builder.}

@defproc[(make-vector-builder
          [initial-contents (sequence/c any/c) empty-list]
          [#:expected-size expected-size (or/c natural? #f) #f])
         unused-vector-builder/c]{
 Constructs a new @tech{vector builder} containing @racket[initial-contents].
 If @racket[expected-size] is given, the builder uses it as a hint of how many
 elements it will eventually contain and adjusts its reserved space accordingly.
 The @racket[expected-size] paramater need only be an estimate; an incorrect
 guess will never cause the builder to produce different results. However, an
 accurate guess will allow the builder to minimize allocations and wasted
 space.}

@defproc[(vector-builder-add [builder unused-vector-builder/c] [v any/c] ...)
         unused-vector-builder/c]{
 Adds each @racket[v] to @racket[builder] and returns an updated @tech{vector
  builder}. The original @racket[builder] is consumed in the process and must
 not be reused.

 @(examples
   #:eval (make-evaluator) #:once
   (define builder (make-vector-builder))
   (vector-builder-add builder 1 2 3)
   (eval:error (vector-builder-add builder 4 5 6)))}

@defproc[(vector-builder-add-all [builder unused-vector-builder/c]
                                 [seq (sequence/c any/c)])
         unused-vector-builder/c]{
 Adds each element of @racket[seq] to @racket[builder] and returns an updated
 @tech{vector builder}. The original @racket[builder] is consumed in the process
 and must not be reused.

 @(examples
   #:eval (make-evaluator) #:once
   (define builder (make-vector-builder))
   (vector-builder-add-all builder (in-range 0 20))
   (eval:error (vector-builder-add-all builder (in-range 20 40))))}

@defproc[(build-vector [builder unused-vector-builder/c]) immutable-vector?]{
 Builds an @tech{immutable vector} from @racket[builder], consuming the builder
 in the process.

 @(examples
   #:eval (make-evaluator) #:once
   (define builder
     (vector-builder-add-all (make-vector-builder) (in-range 0 10)))
   (build-vector builder)
   (eval:error (build-vector builder)))}

@defproc[(build-mutable-vector [builder unused-vector-builder/c])
         (and/c vector? (not/c immutable?))]{
 Builds a mutable @tech/reference{vector} from @racket[builder], consuming the
 builder in the process.

 @(examples
   #:eval (make-evaluator) #:once
   (define builder
     (vector-builder-add-all (make-vector-builder) (in-range 0 10)))
   (build-mutable-vector builder)
   (eval:error (build-mutable-vector builder)))}
