#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/permutation)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/permutation)
    #:private (list 'racket/base)))

@title{Permutations}
@defmodule[rebellion/permutation]

A @deftech{permutation} is a rearrangement of a finite set, with elements of the
set represented by natural numbers. More formally, a permutation of size
@racket[_n] is a bijection from the set of natural numbers less than @racket[_n]
to itself. Permutations are useful for efficiently expressing how collections
are arranged, ordered, and sorted. Two permutations are @racket[equal?] if they
have the same size and rearrange elements in the same way.

@defproc[(permutation? [v any/c]) boolean?]{
 A predicate for @tech{permutations}.}

@defform[(permutation position ...)]{
 Constructs a @tech{permutation} that moves the @racket[_n]th item in a
 collection to the @racket[_n]th @racket[position], with indices starting from
 zero. The resulting permutation's size is equal to the number of @racket[
 position]s given, and no duplicates are allowed. This form is equivalent to the
 standard @deftech{one-line notation} for permutations in mathematics.

 @(examples
   #:eval (make-evaluator) #:once
   (string-permute "abcd" (permutation 0 2 1 3))
   (string-permute "abcd" (permutation 3 2 1 0)))}

@defform[(cyclic-permutation position ...)]{
 Constructs a @tech{permutation} that moves the item at each @racket[position]
 to the next @racket[position] immediately following it. The item at the last
 @racket[position] is moved to the first @racket[position]. The resulting
 permutation's size is equal to one plus the largest @racket[position] given,
 and no duplicates are allowed. This form is equivalent to the standard
 @deftech{cyclic notation} for permutations in mathematics.

 @(examples
   #:eval (make-evaluator) #:once
   (string-permute "abcdefghijklmnopqrstuvwxyz"
                   (cyclic-permutation 5 17 25 8 0)))}

@defthing[empty-permutation permutation? #:value (permutation)]{
 The empty permutation, which rearranges nothing. The size of the empty
 permutation is zero.}

@defproc[(permutation-size [perm permutation?]) natural?]{
 Returns the size of @racket[perm].

 @(examples
   #:eval (make-evaluator) #:once
   (permutation-size (permutation 0 1 2 3 4 5))
   (permutation-size (cyclic-permutation 6 18 4 3 11)))}

@defproc[(permutation-ref [perm permutation?]
                          [pos (and/c natural? (</c (permutation-size perm)))])
         (and/c natural? (</c (permutation-size perm)))]{
 Returns the resulting position of item @racket[pos] when permuted by @racket[
 perm].

 @(examples
   #:eval (make-evaluator) #:once
   (define p (permutation 3 8 2 7 1 6 0 5 4))
   (permutation-ref p 0)
   (permutation-ref p 3)
   (permutation-ref p 8))}

@defproc[(vector-permute [vec (and/c vector? immutable?)] [perm permutation?])
         (and/c vector? immutable?)]{
 Rearranges the elements of @racket[vec] according to @racket[perm], which must
 have the same size as @racket[vec].

 @(examples
   #:eval (make-evaluator) #:once
   (vector-permute (vector-immutable 'a 'b 'c 'd 'e 'f)
                   (permutation 4 1 5 3 0 2)))}

@defproc[(string-permute [str (and/c string? immutable?)] [perm permutation?])
         (and/c string? immutable?)]{
 Rearranges the characters of @racket[str] according to @racket[perm], which
 must have the same size as @racket[str].

 @(examples
   #:eval (make-evaluator) #:once
   (string-permute "abcdefg" (cyclic-permutation 4 5 6)))}

@defproc[(list-permute [xs list?] [perm permutation?])
         list?]{
 Rearranges the elements of @racket[xs] according to @racket[perm], which
 must have the same size as @racket[xs].

 @(examples
   #:eval (make-evaluator) #:once
   (list-permute '(a b c d e f g) (cyclic-permutation 4 5 6)))}
