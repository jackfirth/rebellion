#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     racket/set
                     rebellion/base/comparator
                     rebellion/base/range
                     rebellion/collection/range-set
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/comparator
                   'rebellion/base/range
                   'rebellion/collection/range-set
                   'rebellion/streaming/transducer)
    #:private (list 'racket/base)))


@title{Range Sets}
@defmodule[rebellion/collection/range-set]


A @deftech{range set} is a sorted set of nonempty, disconnected @tech{ranges}. All ranges in the same
range set must use the same @tech{comparator}. Range sets are @tech/reference{sequences}, and when
used as a sequence the set's ranges are iterated in ascending order.


@defproc[(range-set? [v any/c]) boolean?]{
 A predicate for @tech{range sets}.}


@defproc*[([(range-set
             [range nonempty-range?] ...
             [#:comparator comparator comparator?])
            range-set?]
           [(range-set
             [first-range nonempty-range?]
             [range nonempty-range?] ...
             [#:comparator comparator comparator? (range-comparator first-range)])
            range-set?])]{
 Constructs a @tech{range set} containing the given @racket[range]s. If at least one @racket[range] is
 provided, @racket[comparator] may be omitted and defaults to the comparator of the first range.
 Overlapping ranges are disallowed, but adjacent ranges are accepted and are merged together. All
 ranges must use @racket[comparator] as their endpoint comparators or a contract exception is raised.

 @(examples
   #:eval (make-evaluator) #:once
   (range-set (closed-open-range 2 5))
   (range-set (closed-open-range 4 8) (closed-open-range 0 4))
   (range-set (greater-than-range 20) (less-than-range 5) (closed-range 10 15))
   (range-set (closed-open-range 2 5) #:comparator real<=>))}


@defproc[(empty-range-set? [v any/c]) boolean?]{
 A predicate for empty @tech{range sets}. Implies @racket[range-set?].}


@defthing[empty-range-set empty-range-set?]{
 The empty @tech{range set}, which contains no ranges.}


@defproc[(nonempty-range-set? [v any/c]) boolean?]{
 A predicate for nonempty @tech{range sets}. Implies @racket[range-set?].}


@section{Querying Range Sets}


@defproc[(range-set-size [ranges range-set?]) natural?]{
 Returns the number of ranges in @racket[ranges]

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-size (range-set (closed-range 3 5) (closed-range 8 14))))}


@defproc[(range-set-comparator [ranges range-set?]) comparator?]{
 Returns the comparator that @racket[ranges] uses to compare elements to its contained ranges.}


@defproc[(range-set-contains? [ranges range-set?] [value any/c]) boolean?]{
 Determines if any range in @racket[ranges] contains @racket[value].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 9 10))))
   (range-set-contains? ranges 2)
   (range-set-contains? ranges 7)
   (range-set-contains? ranges 10))}


@defproc[(range-set-encloses? [ranges range-set?] [other-range range?]) boolean?]{
 Determines if any range in @racket[ranges] encloses @racket[other-range].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 9 10))))
   (range-set-encloses? ranges (closed-range 3 4))
   (range-set-encloses? ranges (closed-range 4 10))
   (range-set-encloses? ranges (open-range 9 10)))}


@defproc[(range-set-encloses-all? [ranges range-set?] [other-ranges (sequence/c range?)]) boolean?]{
 Determines if @racket[ranges] encloses every range in @racket[other-ranges].

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-encloses-all? (range-set (closed-range 2 5) (closed-range 7 10))
                            (range-set (closed-range 3 4) (closed-range 8 9))))}


@defproc[(range-subset [range-set range-set?] [subset-range range?]) range-set?]{
 Returns a range set containing the ranges in @racket[range-set] that are enclosed by
 @racket[subset-range].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 7 10))))
   (range-subset ranges (less-than-range 4)))}


@section{Range Set Iterations and Comprehensions}


@defproc[(in-range-set [range-set range-set?]) (sequence/c nonempty-range?)]{
 Returns a @tech/reference{sequence} iterating through the ranges in @racket[range-set] in ascending
 order. Note that @tech{range sets} are already sequences, but this form may yield better performance
 when used directly within a @racket[for] clause.}


@defproc[(into-range-set [comparator comparator?]) (reducer/c nonempty-range? range-set?)]{
 Constructs a @tech{reducer} that reduces a sequence of nonempty ranges (which must use
 @racket[comparator]) into a @tech{range set}. As in the @racket[range-set] constructor, overlapping
 ranges are disallowed.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list (closed-range 1 3) (closed-range 10 12) (closed-range 5 6))
              #:into (into-range-set real<=>)))}


@defproc[(sequence->range-set
          [ranges (sequence/c nonempty-range?)]
          [#:comparator comparator comparator?])
         range-set?]{
 Constructs a @tech{range set} from the given @racket[ranges] (which must use @racket[comparator].) As
 in the @racket[range-set] consturctor, overlapping ranges are disallowed.

 @(examples
   #:eval (make-evaluator) #:once
   (sequence->range-set
    (list (closed-range 1 3) (closed-range 10 12) (closed-range 5 6))
    #:comparator real<=>))}


@defform[(for/range-set #:comparator comparator (for-clause ...) body-or-break ... body)]{
 Like @racket[for], but collects the iterated ranges into a @tech{range set}.

 @(examples
   #:eval (make-evaluator) #:once
   (for/range-set #:comparator real<=> ([char (in-string "abcxyz")])
     (define codepoint (char->integer char))
     (closed-open-range codepoint (add1 codepoint))))}


@defform[(for*/range-set #:comparator comparator (for-clause ...) body-or-break ... body)]{
 Like @racket[for*], but collects the iterated ranges into a @tech{range set}.

 @(examples
   #:eval (make-evaluator) #:once
   (for*/range-set #:comparator real<=>
     ([str (in-list (list "abc" "tuv" "xyz" "qrs"))]
      [char (in-string str)])
     (define codepoint (char->integer char))
     (closed-open-range codepoint (add1 codepoint))))}
