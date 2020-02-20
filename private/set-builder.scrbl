#lang scribble/manual

@(require (for-label racket/contract/base
                     racket/set
                     racket/sequence
                     rebellion/collection/list
                     rebellion/collection/set/builder)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/set/builder)
    #:private (list 'racket/base)))

@title{Set Builders}
@defmodule[rebellion/collection/set/builder]

A @deftech{set builder} is a single-use object for building a set. First create
a builder with @racket[make-set-builder], then repeatedly add elements to it
with @racket[set-builder-add] or @racket[set-builder-add-all], and finally
construct a set using either @racket[build-set] or @racket[build-mutable-set].

Set builders are linear objects. Each operation on a set builder destructively
@emph{uses} the builder and returns an updated, unused builder. Used builders
cannot be reused; attempting to do so @emph{may} raise a contract error, but
this is not guaranteed.

@(examples
  #:eval (make-evaluator) #:once
  (define builder (make-set-builder))
  (define builder2 (set-builder-add builder 1 2 3))
  (eval:error (set-builder-add builder 1 2 3)))

@defproc[(set-builder? [v any/c]) boolean?]{
 A predicate for @tech{set builders}.}

@defthing[unused-set-builder/c flat-contract?]{
 A @tech/reference{flat contract} for @tech{set builders} that are @emph{
  unused}. Passing a set builder as an input to functions like @racket[
 set-builder-add] and @racket[build-set] @emph{uses} the builder, and the
 @racket[unused-set-builder/c] contract can detect when a builder has already
 been used. Detection is not guaranteed, because those functions may or may not
 choose to mutate the original builder and reuse it instead of allocating a new
 builder.}

@defproc[(make-set-builder [initial-contents (sequence/c any/c) empty-list])
         unused-set-builder/c]{
 Constructs a new @tech{set builder} containing @racket[initial-contents].}
