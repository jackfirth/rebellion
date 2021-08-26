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


A @deftech{vector builder} is a mutable object for creating vectors. Values can be added to a builder
incrementally with @racket[vector-builder-add], and vectors can be built from a builder with
@racket[build-vector]. Builders can be reused --- a single builder can build many vectors, and
elements can be added to the builder after its already built vectors. Each built vector contains at
least as many elements as every vector built before it.

Vector builders try to efficiently minimize allocation when adding more elements to a builder.
Constructing a vector of size N using a builder consumes an amount of space linear in N.


@defproc[(vector-builder? [v any/c]) boolean?]{
 A predicate for @tech{vector builders}.}


@defproc[(make-vector-builder
          [initial-contents (sequence/c any/c) empty-list]
          [#:expected-size expected-size (or/c natural? #f) #f])
         vector-builder?]{

 Constructs a new @tech{vector builder} containing @racket[initial-contents]. If
 @racket[expected-size] is given, the builder uses it as a hint of how many elements it will
 eventually contain and adjusts its reserved space accordingly. The @racket[expected-size] paramater
 need only be an estimate; an incorrect guess will never cause the builder to produce different
 results. However, an accurate guess will allow the builder to minimize wasted space.}


@defproc[(vector-builder-add [builder vector-builder?] [v any/c] ...) vector-builder?]{

 Adds each @racket[v] to @racket[builder] and returns @racket[builder].
 @bold{This mutates the builder!} The builder is returned as a convenience to the caller when used
 with operations like @racket[for/fold].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define builder (make-vector-builder)))

   (vector-builder-add builder 1 2 3)
   (vector-builder-add builder 4 5 6)
   (build-vector builder))}


@defproc[(vector-builder-add-all [builder vector-builder?] [elements (sequence/c any/c)])
         vector-builder?]{

 Adds @racket[elements] to @racket[builder] and returns @racket[builder].
 @bold{This mutates the builder!} The builder is returned as a convenience to the caller when used
 with operations like @racket[for/fold].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define builder (make-vector-builder)))
   
   (vector-builder-add-all builder (in-range 0 5))
   (vector-builder-add-all builder (in-range 5 10))
   (build-vector builder))}


@defproc[(build-vector [builder vector-builder?]) immutable-vector?]{

 Builds an @tech{immutable vector} from @racket[builder]. Does not mutate @racket[builder] in any way,
 and @racket[builder] can still be used to build additional vectors afterwards.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define builder (make-vector-builder)))
   
   (build-vector (vector-builder-add builder 1 2 3))
   (build-vector (vector-builder-add builder 4 5 6)))}


@defproc[(build-mutable-vector [builder vector-builder?]) (and/c vector? (not/c immutable?))]{

 Builds a mutable @tech/reference{vector} from @racket[builder]. Does not mutate @racket[builder] in
 any way, and @racket[builder] can still be used to build additional vectors afterwards.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define builder (make-vector-builder)))
   
   (build-mutable-vector (vector-builder-add builder 1 2 3))
   (build-mutable-vector (vector-builder-add builder 4 5 6)))}


@section{Legacy APIs}


@defthing[unused-vector-builder/c predicate/c]{

 @bold{Deprecated.} Alias for @racket[vector-builder?], use that instead.}
