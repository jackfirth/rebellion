#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/set
                     rebellion/collection/keyset)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/set
                   'rebellion/collection/keyset)
    #:private (list 'racket/base)))

@title{Keysets}
@defmodule[rebellion/collection/keyset]

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

@defproc[(keyset-add [keys keyset?] [kw keyword?]) keyset?]{
 Adds @racket[kw] to @racket[keys], returning the modified @tech{keyset}.

 @(examples
   #:eval (make-evaluator) #:once
   (define fruits (keyset #:banana #:orange #:apple #:grape))
   (keyset-add fruits '#:peach))}

@defproc[(keyset-remove [keys keyset?] [kw keyword?]) keyset?]{
 Removes @racket[kw] from @racket[keys], returning the modified @tech{keyset}.

 @(examples
   #:eval (make-evaluator) #:once
   (define fruits (keyset #:banana #:orange #:apple #:grape))
   (keyset-remove fruits '#:banana))}

@defproc[(keyset->list [keys keyset?]) (listof keyword?)]{
 Converts @racket[keys] into a plain list of keywords.

 @(examples
   #:eval (make-evaluator) #:once
   (define greek-gods (keyset #:zeus #:hera #:hades #:athena #:poseidon))
   (keyset->list greek-gods))}

@defproc[(list->keyset [kws (listof keyword?)]) keyset?]{
 Sorts @racket[kws] and removes duplicates, returning a @tech{keyset}.

 @(examples
   #:eval (make-evaluator) #:once
   (list->keyset (list '#:banana '#:orange '#:orange '#:apple '#:grape)))}

@defproc[(keyset->set [keys keyset?]) (immutable-set/c keyword?)]{
 Converts @racket[keys] into a plain @tech{set} of keywords.

 @(examples
   #:eval (make-evaluator) #:once
   (keyset->set (keyset #:red #:blue #:green #:yellow)))}

@defproc[(set->keyset [kw-set (immutable-set/c keyword?)]) keyset?]{
 Sorts @racket[kw-set], returning a @tech{keyset}.

 @(examples
   #:eval (make-evaluator) #:once
   (set->keyset (set '#:red '#:blue '#:green '#:yellow)))}
