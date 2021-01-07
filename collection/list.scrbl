#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/base/option
                     rebellion/collection/list
                     rebellion/streaming/reducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/list
                   'rebellion/streaming/reducer)
    #:private (list 'racket/base)))

@title{Lists}
@defmodule[rebellion/collection/list]

The @racketmodname[rebellion/collection/list] library is a small wrapper module
designed to provide a more modern and conventional interface to standard Racket
lists. Additionally, it provides a few utilities for integrating lists with some
of Rebellion's standard abstractions, such as @tech{reducers}. No attempt is
made to wrap every possible list-related function or rename every export of
@racketmodname[racket/list], as many of these operations are better expressed in
terms of generic @tech/reference{sequences}, @racket[for] loops, @tech{
 reducers}, and @tech{transducers} rather than lists specifically.

For the purposes of this library, "list" means "proper list". Improper lists
(such as those constructed by @racket[list*]) are a poor fit for most
list-related tasks and should be avoided in favor of other data structures.

@defproc[(empty-list? [v any/c]) boolean?]{
 A predicate for empty lists. Implies @racket[list?].}

@defproc[(nonempty-list? [v any/c]) boolean?]{
 A predicate for nonempty, proper lists. Implies @racket[list?] and is mutually
 exclusive with @racket[empty-list?].}

@defproc[(list-first [lst nonempty-list?]) list?]{
 Returns the first element of @racket[lst]. Unlike @racket[list-ref-safe], this
 function does not return an @tech{option} and instead requires that @racket[
 lst] not be empty.

 @(examples
   #:eval (make-evaluator) #:once
   (list-first (list 1 2 3))
   (eval:error (list-first empty-list)))}

@defproc[(list-rest [lst nonempty-list?]) list?]{
 Removes the first element from @racket[lst] and returns the list of remaining
 elements. If @racket[lst] is empty, a contract failure is raised.

 @(examples
   #:eval (make-evaluator) #:once
   (list-rest (list 1 2 3))
   (eval:error (list-rest empty-list)))}

@defproc[(list-ref-safe [lst list?] [position natural?]) option?]{
 Returns an @tech{option} containing the value at @racket[position] in
 @racket[lst], or @racket[absent] if @racket[lst] is too small to contain an
 element at @racket[position]. This operation takes time linear in the length of
 the list.

 @(examples
   #:eval (make-evaluator) #:once
   (list-ref-safe (list 'a 'b 'c) 0)
   (list-ref-safe (list 'a 'b 'c) 2)
   (list-ref-safe (list 'a 'b 'c) 5))}

@defproc[(list-size [lst list?]) natural?]{
 Returns the number of elements in @racket[lst]. This function calls @racket[
 length] from @racketmodname[racket/base], which may be faster than linear time
 in certain cases. Nevertheless, do not assume this is a constant-time
 operation.

 @(examples
   #:eval (make-evaluator) #:once
   (list-size (list 'a 'b 'c))
   (list-size empty-list))}

@defproc[(list-insert [lst list?] [v any/c]) nonempty-list?]{
 Inserts @racket[v] into the front of @racket[lst] and returns a new, updated
 list. This is intended to replace @racket[cons], but note that the argument
 order is different to follow the convention of core operations on a data type
 taking that type as the first argument.

 @(examples
   #:eval (make-evaluator) #:once
   (list-insert (list 2 3 4) 1))}

@defproc[(list-contains? [lst list?] [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[lst] contains @racket[v], returns @racket[#f]
 otherwise. This operation takes time linear in the length of the list. Lists
 are not designed for efficient membership queries, so if you find yourself
 using this function consider whether you could use a @tech/reference{set} or
 @tech{multiset} instead.

 @(examples
   #:eval (make-evaluator) #:once
   (list-contains? (list 1 2 3 4 5) 2)
   (list-contains? (list 1 2 3 4 5) 1000))}

@defproc[(list-append [lst list?] ...) list?]{
 Appends each @racket[lst] together into a single, flattened list.

 @(examples
   #:eval (make-evaluator) #:once
   (list-append (list 1 2 3) empty-list (list 4 5) (list 6 7 8 9))
   (list-append (list 1 2 3))
   (list-append))}

@defproc[(list-reverse [lst list?]) list?]{
 Reverses the order of elements in @racket[lst].

 @(examples
   #:eval (make-evaluator) #:once
   (list-reverse (list 1 2 3 4 5))
   (list-reverse empty-list))}

@defthing[empty-list empty-list? #:value (list)]{
 The empty list, provided as a constant. Prefer using this constant over writing
 @racket[(list)] or @racket['()]. The former obscures that a constant value is
 intended, and the latter unnecessarily requires the reader to understand the
 subtleties of quotation.}

@defthing[into-list (reducer/c any/c list?)]{
 A @tech{reducer} that collects elements into a list, preserving their order.

 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-list 1 2 3 4 5))}

@defthing[into-reversed-list (reducer/c any/c list?)]{
 A @tech{reducer} that collects elements into a list, in reverse order. This can
 be more efficient than @racket[into-list]. This function should be preferred
 over @racket[in-list] when the order of the returned list doesn't matter, but
 in these cases strongly consider using @tech/reference{sets} or @tech{
  multisets} instead of lists to make the lack of order explicit.

 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-reversed-list 1 2 3 4 5))}

@defthing[append-into-list (reducer/c list? list?)]{
 A @tech{reducer} that collects a sequence of lists into a single list, in the
 same manner as @racket[list-append].

 @(examples
   #:eval (make-evaluator) #:once
   (reduce append-into-list
           (list 1 2 3)
           (list 'a 'b)
           empty-list
           (list 4 5 6 7 8)))}
