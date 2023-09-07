#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/vector
                     rebellion/permutation
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public
    (list 'rebellion/collection/vector
          'rebellion/permutation
          'rebellion/streaming/reducer
          'rebellion/streaming/transducer)
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
   (permute "abcd" (permutation 0 2 1 3) #:into into-string)
   (permute "abcd" (permutation 3 2 1 0) #:into into-string))}

@defform[(cyclic-permutation position ...)]{
 Constructs a @tech{permutation} that moves the item at each @racket[position]
 to the next @racket[position] immediately following it. The item at the last
 @racket[position] is moved to the first @racket[position]. The resulting
 permutation's size is equal to one plus the largest @racket[position] given,
 and no duplicates are allowed. This form is equivalent to the standard
 @deftech{cyclic notation} for permutations in mathematics.

 @(examples
   #:eval (make-evaluator) #:once
   (permute "abcdefghijklmnopqrstuvwxyz"
            (cyclic-permutation 5 17 25 8 0)
            #:into into-string))}

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

@defproc[(permute
          [seq (sequence/c any/c)]
          [perm permutation?]
          [#:into reducer reducer? (into-vector)])
         any/c]{
 Rearranges the elements of @racket[seq] according to @racket[perm], which must have the same size as
 @racket[seq] has elements. The rearranged elements are collected into @racket[reducer] which defaults
 to a reduction into a vector.

 @(examples
   #:eval (make-evaluator) #:once
   (permute '(a b c d e f) (permutation 4 1 5 3 0 2))
   (permute "abcdef" (permutation 4 1 5 3 0 2) #:into into-string))}


@defproc[(permuting [perm permutation?]) transducer?]{
 Constructs a @tech{transducer} that rearranges the upstream elements according to @racket[perm] and
 emits them downstream. Elements are emitted as soon as they're available. The transduction consumes
 memory linear in the size of the permutation, but references to elements are not kept after they've
 been emitted downstream.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce "abcdef"
              (permuting (permutation 5 4 3 2 1 0))
              #:into into-string))}

@defproc[(permutation-reverse [perm permutation?]) permutation?]{
 Constructs the inverse permutation of @racket[perm], which permutes elements in exactly the opposite
 manner as @racket[perm]. The inverse of a permutation effectively undoes that permutation.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define shift-right-once (permutation 1 2 3 4 5 0)))
   
   (permute "abcdef" shift-right-once #:into into-string)
   (permute "abcdef" (permutation-reverse shift-right-once) #:into into-string))}
