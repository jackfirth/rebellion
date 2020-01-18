#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/set
                     rebellion/base/immutable-string
                     rebellion/collection/set
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/immutable-string
                   'rebellion/collection/set
                   'rebellion/streaming/reducer
                   'rebellion/streaming/transducer)
    #:private (list 'racket/base)))

@title{Sets}
@defmodule[rebellion/collection/set]

@defthing[empty-set emtpy-set?]{
 The empty immutable @tech/reference{set}.}

@defproc[(empty-set? [v any/c]) boolean?]{
 A predicate for empty immutable sets. Implies @racket[set?].}

@defproc[(nonempty-set? [v any/c]) boolean?]{
 A predicate for nonempty immutable sets. Implies @racket[set?].}

@defproc[(mutable-set? [v any/c]) boolean?]{
 A predicate for mutable sets. Equivalent to @racket[set-mutable?].}

@deftogether[[
 @defthing[into-set (reducer/c any/c set?)]
 @defthing[into-mutable-set (reducer/c any/c set-mutable?)]]]{
 @tech{Reducers} that collect elements of the reduced sequence into either an
 immutable @tech/reference{set} or a mutable set, respectively.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list "the" "quick" "brown" "fox")
              (mapping immutable-string-length)
              #:into into-set))}
