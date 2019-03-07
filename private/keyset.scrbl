#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/keyset)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/keyset)
    #:private (list 'racket/base)))

@title{Keysets}
@defmodule[rebellion/keyset]

A @deftech{keyset} is a data structure representing an immutable sorted set of
keywords. Keysets can be more efficient than generic sorted set implementations.
For example, the @racket[keyset] constructor is a macro that sorts the keywords
at compile-time.

@defproc[(keyset? [v any/c]) boolean?]{
 A predicate for @tech{keysets}.}

@defform[(keyset keyword ...)]{
 Constructs a @tech{keyset} containing each @racket[keyword], excluding
 duplicates. The keywords are sorted by @racket[keyword<?] at compile-time, when
 the @racket[keyset] form is expanded. The expanded code of a @racket[keyset]
 form runs in linear time.

 @(examples
   #:eval (make-evaluator) #:once
   (keyset #:banana #:orange #:apple #:grape)
   (keyset #:orange #:banana #:banana))}

@defthing[empty-keyset keyset? #:value (keyset)]{
 The empty @tech{keyset}, which contains no keywords.}

@defproc[(keyset-contains? [keys keyset?] [kw keyword?]) boolean?]{
 Returns @racket[#t] if @racket[kw] is present in @racket[keys]. This is a
 constant-time operation.

 @(examples
   #:eval (make-evaluator) #:once
   (define fruits (keyset #:banana #:orange #:apple #:grape))
   (keyset-contains? fruits '#:orange)
   (keyset-contains? fruits '#:walnut))}

@defproc[(keyset-index-of [keys keyset?] [kw keyword?]) natural?]{
 Returns the position of @racket[kw] in @racket[keys]. This is a constant-time
 operation.

 @(examples
   #:eval (make-evaluator) #:once
   (define fruits (keyset #:banana #:orange #:apple #:grape))
   (keyset-index-of fruits '#:grape)
   (keyset-index-of fruits '#:banana))}

@defproc[(keyset-ref [keys keyset?] [pos natural?]) keyword?]{
 Returns the keyword at position @racket[pos] in @racket[keys]. This is a
 constant-time operation.

 @(examples
   #:eval (make-evaluator) #:once
   (define fruits (keyset #:banana #:orange #:apple #:grape))
   (keyset-ref fruits 0)
   (keyset-ref fruits 3))}

@defproc[(keyset-size [keys keyset?]) natural?]{
 Returns the number of keywords in @racket[keys]. This is a constant-time
 operation.

 @(examples
   #:eval (make-evaluator) #:once
   (define fruits (keyset #:banana #:orange #:apple #:grape))
   (keyset-size fruits))}
