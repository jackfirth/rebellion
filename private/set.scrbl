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

@deftogether[[
 @defthing[into-set reducer?]
 @defthing[into-mutable-set reducer?]]]{
 @tech{Reducers} that collect elements of the reduced sequence into either an
 immutable @tech/reference{set} or a mutable set, respectively.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list "the" "quick" "brown" "fox")
              (mapping immutable-string-length)
              #:into into-set))}
