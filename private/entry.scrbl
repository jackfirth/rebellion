#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/collection/entry)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/entry)
    #:private (list 'racket/base)))

@title{Entries}
@defmodule[rebellion/collection/entry]

An @deftech{entry} is a key-value pair representing a mapping in a
dictionary-like collection. Entries are semantically equivalent to @tech{pairs},
but use the less generic and more readable names @racket[entry-key] and @racket[
 entry-value]. Use entries instead of pairs when working with dicitonary-like
types, as a collection of @racket[entry?] values has a clearer intended purpose
than a collection of @racket[pair?] values.

@defproc[(entry? [v any/c]) boolean?]{
 A predicate for @tech{entries}.}

@defproc[(entry [k any/c] [v any/c]) entry?]{
 Constructs an @tech{entry}.

 @(examples
   #:eval (make-evaluator) #:once
   (entry "apple" 'red)
   (entry "banana" 'yellow))}

@defproc[(entry-key [e entry?]) any/c]{
 Returns the key of @racket[e].

 @(examples
   #:eval (make-evaluator) #:once
   (entry-key (entry "apple" 'red)))}

@defproc[(entry-value [e entry?]) any/c]{
 Returns the value in @racket[e].

 @(examples
   #:eval (make-evaluator) #:once
   (entry-value (entry "banana" 'yellow)))}
