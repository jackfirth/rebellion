#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/generative-token)
          scribble/example)

@(define module-sharing-evaluator-factory
   (make-base-eval-factory (list 'racket/base 'rebellion/generative-token)))

@(define (make-evaluator)
   (define evaluator (module-sharing-evaluator-factory))
   (evaluator '(require rebellion/generative-token))
   evaluator)

@title{Generative Tokens}
@defmodule[rebellion/generative-token]

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
