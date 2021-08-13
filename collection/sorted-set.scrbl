#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     rebellion/base/comparator
                     rebellion/collection/sorted-set)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/comparator
                   'rebellion/collection/sorted-set)
    #:private (list 'racket/base)))


@title{Sorted Sets}
@defmodule[rebellion/collection/sorted-set]


A @deftech{sorted set} is a @tech{collection} of distinct elements sorted according to some
@tech{comparator}. Sorted sets may be mutable, immutable, or @tech{unmodifiable}. Two immutable sorted
sets are @racket[equal?] if and only if they contain the same elements and use @racket[equal?]
comparators. Two mutable sorted sets are @racket[equal?] if and only if they will @emph{always}
contain the same elements and use @racket[equal?] comparators, meaning that they share the same
mutable state. This is not necessarily the same as being @racket[eq?], as some mutable sets may be
views of others.

All sorted sets are @tech/reference{sequences}. When iterated, a sorted set traverses its elements in
ascending order as defined by its comparator. To traverse a sorted set in descending order, either use
@racket[in-sorted-set] with @racket[#:descending?] set to true, or reverse the sorted set with
@racket[sorted-set-reverse]. Note that @racket[sorted-set-reverse] returns a view of the original set,
not a copy, so it constructs the view in constant time regardless of the size of the original set.


@defproc[(sorted-set? [v any/c]) boolean?]{
 A predicate for @tech{sorted sets}. Includes both mutable, immutable sorted sets.}


@defproc[(mutable-sorted-set? [v any/c]) boolean?]{
 A predicate for mutable @tech{sorted sets}. Implies @racket[sorted-set?].}


@defproc[(immutable-sorted-set? [v any/c]) boolean?]{
 A predicate for immutable @tech{sorted sets}. Implies @racket[sorted-set?].}


@section{Constructing Sorted Sets}


@defproc[(sorted-set [element any/c] ... [#:comparator comparator comparator?])
         immutable-sorted-set?]{

 Constructs an immutable @tech{sorted set} containing @racket[element]s sorted by @racket[comparator].
 The input @racket[element]s may be given in any order, and duplicate elements (as in, elements that
 @racket[comparator] considers equivalent) are ignored.

 @(examples
   #:eval (make-evaluator) #:once
   (sorted-set 8 2 5 7 #:comparator natural<=>))}


@defproc[(sequence->sorted-set [sequence (sequence/c any/c)] [#:comparator comparator comparator?])
         immutable-sorted-set?]{

 Copies @racket[sequence] into an immutable @tech{sorted set} sorted by @racket[comparator]. Duplicate
 elements of @racket[sequence] (as in, elements that @racket[comparator] considers equivalent) are
 ignored.

 The @racket[sequence->sorted-set] function makes an effort to avoid unnecessary copying if its input
 is already an immutable sorted set that's sorted according to @racket[comparator]. The precise
 details of this optimization are not guaranteed, but in general, converting a sequence to a sorted
 set and then converting it again takes the same amount of time as converting it once.

 @(examples
   #:eval (make-evaluator) #:once
   (sequence->sorted-set "hello world" #:comparator char<=>))}
