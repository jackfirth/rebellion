#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     rebellion/base/comparator
                     rebellion/base/option
                     rebellion/collection/sorted-set
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/comparator
                   'rebellion/collection/sorted-set
                   'rebellion/streaming/reducer
                   'rebellion/streaming/transducer)
    #:private (list 'racket/base)))


@title{Sorted Sets}
@defmodule[rebellion/collection/sorted-set]


A @deftech{sorted set} is a @tech{collection} of distinct elements sorted according to some
@tech{comparator}. Sorted sets may be mutable, immutable, or @tech{unmodifiable}. Two immutable sorted
sets are @racket[equal?] if and only if they contain the same elements and use @racket[equal?]
comparators. Two mutable sorted sets are @racket[equal?] if and only if they will @emph{always}
contain the same elements and use @racket[equal?] comparators, meaning that they share the same
mutable state. This is not necessarily the same as being @racket[eq?], as some sorted sets may be
views of others.

All sorted sets are @tech/reference{sequences}. When iterated, a sorted set traverses its elements in
ascending order as defined by its comparator. To traverse a sorted set in descending order, either use
@racket[in-sorted-set] with @racket[#:descending?] set to true, or reverse the sorted set with
@racket[sorted-set-reverse]. Note that @racket[sorted-set-reverse] returns a view of the original set,
not a copy, so it constructs the view in constant time regardless of the size of the original set.


@defproc[(sorted-set? [v any/c]) boolean?]{
 A predicate for @tech{sorted sets}. Includes mutable, immutable, and @tech{unmodifiable} sorted
 sets.}


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
   (sequence->sorted-set (list 4 8 2 1) #:comparator natural<=>)
   (sequence->sorted-set "hello world" #:comparator char<=>))}


@defproc[(into-sorted-set [comparator comparator?]) (reducer/c any/c immutable-sorted-set?)]{
                                                                                             
 Constructs a @tech{reducer} that reduces elements into an immutable @tech{sorted set} sorted by
 @racket[comparator]. Duplicate elements (as in, elements that @racket[comparator] considers
 equivalent) are ignored.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list 4 8 2 1) #:into (into-sorted-set natural<=>)))}


@defproc[(make-mutable-sorted-set
          [initial-elements (sequence/c any/c) '()] [#:comparator comparator comparator?])
         mutable-sorted-set?]{

 Constructs a new mutable @tech{sorted set} containing @racket[initial-elements] (which defaults to
 the empty list) sorted according to @racket[comparator]. Duplicate elements in
 @racket[initial-elements] (as in, elements that @racket[comparator] considers equivalent) are
 ignored.

 @(examples
   #:eval (make-evaluator) #:once
   (make-mutable-sorted-set #:comparator natural<=>)
   (make-mutable-sorted-set (list 4 7 3 5) #:comparator natural<=>))}


@section{Querying Sorted Sets}


@defproc[(in-sorted-set [set sorted-set?] [#:descending? descending? boolean? #false])
         (sequence/c any/c)]{

 Returns a @tech/reference{sequence} that iterates through the elements of @racket[set] in ascending
 order. If @racket[descending?] is true, the sequence iterates in descending order instead.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define fruits
      (sorted-set "apple" "orange" "grape" "banana"
                  #:comparator string<=>)))

   (transduce (in-sorted-set fruits)
              #:into (join-into-string ", "))

   (transduce (in-sorted-set fruits #:descending? #true)
              #:into (join-into-string ", ")))}


@defproc[(sorted-set-empty? [set sorted-set?]) boolean?]{

 Returns true if @racket[set] contains no elements, returns false otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (sorted-set-empty? (sorted-set #:comparator natural<=>))
   (sorted-set-empty? (sorted-set 1 2 3 #:comparator natural<=>)))}


@defproc[(sorted-set-size [set sorted-set?]) natural?]{

 Returns the number of elements in @racket[set].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sequence->sorted-set (in-range 5 15) #:comparator natural<=>)))
   
   (sorted-set-size numbers))}


@defproc[(sorted-set-comparator [set sorted-set?]) comparator?]{

 Returns the @tech{comparator} used by @racket[set] to sort elements.

 @(examples
   #:eval (make-evaluator) #:once
   (sorted-set-comparator (sorted-set 1 2 3 #:comparator natural<=>)))}


@defproc[(sorted-set-contains? [set sorted-set?] [value any/c]) boolean?]{

 Returns true if @racket[set] contains @racket[value], returns false otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sorted-set 1 2 3 4 5 #:comparator natural<=>)))

   (sorted-set-contains? numbers 2)
   (sorted-set-contains? numbers 100))}


@defproc[(sorted-set-contains-any? [set sorted-set?] [sequence (sequence/c any/c)]) boolean?]{

 Returns true if @racket[set] contains at least one element in @racket[sequence], returns false
 otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sorted-set 1 2 3 4 5 #:comparator natural<=>)))

   (sorted-set-contains-any? numbers (list 1 10 100))
   (sorted-set-contains-any? numbers (vector 11 12 13))
   (sorted-set-contains-any? numbers (list)))}


@(define vacuous-truth
   @hyperlink["https://en.wikipedia.org/wiki/Vacuous_truth"]{vacuous truth})


@defproc[(sorted-set-contains-all? [set sorted-set?] [sequence (sequence/c any/c)]) boolean?]{

 Returns true if @racket[set] contains every element in @racket[sequence], returns false otherwise. If
 @racket[sequence] is empty, then by the principle of @vacuous-truth the result is true.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sorted-set 1 2 3 4 5 #:comparator natural<=>)))

   (sorted-set-contains-all? numbers (list 1 2 3))
   (sorted-set-contains-all? numbers (vector 1 10 100))
   (sorted-set-contains-all? numbers (list)))}


@defproc[(sorted-set-contains-none? [set sorted-set?] [sequence (sequence/c any/c)]) boolean?]{

 Returns true if @racket[set] does not contain any element in @racket[sequence], returns false
 otherwise. If @racket[sequence] is empty, then by the principle of @vacuous-truth the result is true.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sorted-set 1 2 3 4 5 #:comparator natural<=>)))

   (sorted-set-contains-none? numbers (list 11 12 13))
   (sorted-set-contains-none? numbers (vector 1 10 100))
   (sorted-set-contains-none? numbers (list)))}


@defproc[(sorted-set-least-element [set sorted-set?]) option?]{

 Returns the first and smallest element in @racket[set], or @racket[absent] if @racket[set] is empty.

 @(examples
   #:eval (make-evaluator) #:once
   (sorted-set-least-element (sorted-set 5 2 6 #:comparator natural<=>))
   (sorted-set-least-element (sorted-set #:comparator natural<=>)))}


@defproc[(sorted-set-greatest-element [set sorted-set?]) option?]{

 Returns the last and largest element in @racket[set], or @racket[absent] if @racket[set] is empty.

 @(examples
   #:eval (make-evaluator) #:once
   (sorted-set-greatest-element (sorted-set 5 2 6 #:comparator natural<=>))
   (sorted-set-greatest-element (sorted-set #:comparator natural<=>)))}


@defproc[(sorted-set-element-less-than [set sorted-set?] [upper-bound any/c]) option?]{
                                                                                       
 Returns the largest element in @racket[set] less than @racket[upper-bound], or @racket[absent] if no
 such element exists.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sorted-set 5 10 20 #:comparator natural<=>)))
   
   (sorted-set-element-less-than numbers 12)
   (sorted-set-element-less-than numbers 2))}


@defproc[(sorted-set-element-greater-than [set sorted-set?] [lower-bound any/c]) option?]{
                                                                                       
 Returns the smallest element in @racket[set] greater than @racket[lower-bound], or @racket[absent] if
 no such element exists.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sorted-set 5 10 20 #:comparator natural<=>)))
   
   (sorted-set-element-greater-than numbers 7)
   (sorted-set-element-greater-than numbers 20))}


@defproc[(sorted-set-element-at-most [set sorted-set?] [upper-bound any/c]) option?]{
                                                                                       
 Returns the largest element in @racket[set] less than or equivalent to @racket[upper-bound], or
 @racket[absent] if no such element exists.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sorted-set 5 10 20 #:comparator natural<=>)))
   
   (sorted-set-element-at-most numbers 12)
   (sorted-set-element-at-most numbers 10)
   (sorted-set-element-at-most numbers 2))}


@defproc[(sorted-set-element-at-least [set sorted-set?] [lower-bound any/c]) option?]{
                                                                                       
 Returns the smallest element in @racket[set] greater than or equivalent to @racket[lower-bound], or
 @racket[absent] if no such element exists.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define numbers
      (sorted-set 5 10 20 #:comparator natural<=>)))
   
   (sorted-set-element-at-least numbers 7)
   (sorted-set-element-at-least numbers 5)
   (sorted-set-element-at-least numbers 30))}
