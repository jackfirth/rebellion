#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/generative-token)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/generative-token)
    #:private (list 'racket/base)))

@title{Generative Tokens}
@defmodule[rebellion/base/generative-token]

A @deftech{generative token} is a primitive data type for unique objects. Two
generative tokens are only @racket[equal?] if they are @racket[eq?], and the
constructor @racket[make-generative-token] always creates a new token that is
not @racket[eq?] to any other token. Tokens contain no other data --- their only
use is as a building block for creating other, more complex generative data
types.

@defproc[(generative-token? [v any/c]) boolean?]{
 A predicate for @tech{generative tokens}.}

@defproc[(make-generative-token) generative-token?]{
 Constructs a new @tech{generative token} that is distinct from all other
 tokens. Multiple calls to @racket[make-generative-token] always yield multiple
 distinct tokens.

 @(examples
   #:eval (make-evaluator) #:once
   (define tok (make-generative-token))
   tok
   (equal? tok tok)
   (equal? tok (make-generative-token)))}
