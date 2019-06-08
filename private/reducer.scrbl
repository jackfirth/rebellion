#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/streaming/reducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/streaming/reducer)
    #:private (list 'racket/base)))

@title{Reducers}
@defmodule[rebellion/streaming/reducer]

@defproc[(reducer? [v any/c]) boolean?]
@defproc[(reduce [red reducer?] [v any/c] ...) any/c]

@defthing[into-count reducer?]{
 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-count 'a 'b 'c 'd 'e 'f 'g 'h))}
