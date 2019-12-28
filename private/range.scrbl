#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/comparator
                     rebellion/base/range)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/comparator
                   'rebellion/base/range)
    #:private (list 'racket/base)))

@title{Ranges}
@defmodule[rebellion/base/range]

A @deftech{range}, also called an @deftech{interval}, is a continuous set of
ordered values. Ranges have at most two bounds: an upper bound and a lower
bound. Either bound may be absent, in which case the range is @deftech{
 unbounded}. If a bound is present, it contains an endpoint value and an
indication of whether the bound is inclusive or exclusive.

@section{Range Data Model}

@defproc[(range? [v any/c]) boolean?]{
 A predicate for @tech{ranges}.}

@defproc[(range-lower-bound [rng range?]) (or/c range-bound? unbounded?)]{
 Returns the lower bound of @racket[rng], or @racket[unbounded] if @racket[rng]
 does not have a lower bound.

 @(examples
   #:eval (make-evaluator) #:once
   (range-lower-bound (closed-range 3 7))
   (range-lower-bound (open-closed-range 3 7))
   (range-lower-bound (at-least 5))
   (range-lower-bound (less-than 14)))}

@defproc[(range-upper-bound [rng range?]) (or/c range-bound? unbounded?)]{
 Returns the upper bound of @racket[rng], or @racket[unbounded] if @racket[rng]
 does not have an upper bound.

 @(examples
   #:eval (make-evaluator) #:once
   (range-upper-bound (closed-range 3 7))
   (range-upper-bound (open-closed-range 3 7))
   (range-upper-bound (at-least 5))
   (range-upper-bound (less-than 14)))}

@defproc[(range-comparator [rng range?]) comparator?]{
 Returns the @tech{comparator} that @racket[rng] uses to compare values to its
 bounds.}

@defproc[(unbounded? [v any/c]) boolean?]{
 A predicate for the @tech{unbounded} range endpoint constant.}

@defthing[unbounded unbounded?]{
 A constant used with the @racket[range] constructor to indicate that a @tech{
  range} is @tech{unbounded} on one or both ends.}

@defproc[(range-bound? [v any/c]) boolean?]{
 A predicate for @tech{range} bounds, which contain an endpoint value and are
 either inclusive or exclusive.}

@defproc[(range-bound-endpoint [bound range-bound?]) any/c]{
 Returns the endpoint value in @racket[bound].}

@defproc[(range-bound-type [bound range-bound?]) range-bound-type?]{
 Returns the type (inclusive or exclusive) of @racket[bound].}

@defproc[(inclusive-bound [v any/c]) range-bound?]{
 Constructs an @deftech{inclusive} @tech{range} bound, which includes values
 equal to @racket[v]. An inclusive bound is also called a closed bound.}

@defproc[(exclusive-bound [v any/c]) range-bound?]{
 Constructs an @deftech{exclusive} @tech{range} bound, which does not include
 values equal to @racket[v]. An exclusive bound is also called an open bound.}

@defproc[(range-bound-type? [v any/c]) boolean?]{
 A predicate for the two constants @racket[inclusive] and @racket[exclusive].}

@deftogether[[
 @defthing[inclusive range-bound-type?]
 @defthing[exclusive range-bound-type?]]]{
 Constants for the two types of @tech{range} bounds. An inclusive bound includes
 the endpoint value, an exclusive bound does not include the endpoint value.}

@section{Range Constructors}

@defproc[(range [lower-bound (or/c range-bound? unbounded?)]
                [upper-bound (or/c range-bound? unbounded?)]
                [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs a @tech{range} encompassing all values between @racket[lower-bound]
 and @racket[upper-bound], when ordered according to @racket[comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (range (inclusive-bound 3) (exclusive-bound 7)))

 Either @racket[lower-bound] or @racket[upper-bound] may be the special @racket[
 unbounded] constant, indicating that the range is @tech{unbounded} on that end.
 A range may be unbounded on both ends, in which case the range encompasses all
 possible values (as long as they would be accepted by @racket[comparator]).

 @(examples
   #:eval (make-evaluator) #:once
   (range (inclusive-bound 3) unbounded)
   (range unbounded (exclusive-bound 7))
   (range unbounded unbounded))

 If the range is not unbounded, then @racket[lower-bound] must not be greater
 than @racket[upper-bound] when compared with @racket[comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:error (range (inclusive-bound 42) (exclusive-bound 17))))

 However, @racket[lower-bound] @emph{may} be equal to @racket[upper-bound], as
 long as at least one of the two bounds is inclusive. If both bounds are equal
 and inclusive, this constructs a @deftech{singleton range} which contains only
 one value. If one of the bounds is exclusive, the constructed range is an
 @deftech{empty range} that contains no values. If both bounds are exclusive, a
 contract violation is raised.

 @(examples
   #:eval (make-evaluator) #:once
   (range (inclusive-bound 5) (inclusive-bound 5))
   (range (inclusive-bound 18) (exclusive-bound 18))
   (eval:error (range (exclusive-bound 42) (exclusive-bound 42))))}

@defproc[(closed-range [lower-endpoint any/c]
                       [upper-endpoint any/c]
                       [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs a @tech{range} with inclusive lower and upper bounds of @racket[
 lower-endpoint] and @racket[upper-endpoint]. The constructed range contains all
 values between @racket[lower-endpoint] and @racket[upper-endpoint], including
 both endpoints. Values are compared using @racket[comparator].}

@defproc[(open-range [lower-endpoint any/c]
                     [upper-endpoint any/c]
                     [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs a @tech{range} with exclusive lower and upper bounds of @racket[
 lower-endpoint] and @racket[upper-endpoint]. The constructed range contains all
 values between @racket[lower-endpoint] and @racket[upper-endpoint], but does
 not include either endpoint. Values are compared using @racket[comparator].}

@defproc[(closed-open-range [lower-endpoint any/c]
                            [upper-endpoint any/c]
                            [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs a @tech{range} with an inclusive lower bound of @racket[
 lower-endpoint] and an exclusive upper bound of @racket[upper-endpoint]. Values
 are compared using @racket[comparator].}

@defproc[(open-closed-range [lower-endpoint any/c]
                            [upper-endpoint any/c]
                            [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs a @tech{range} with an exclusive lower bound of @racket[
 lower-endpoint] and an inclusive upper bound of @racket[upper-endpoint]. Values
 are compared using @racket[comparator].}

@defproc[(at-least [lower-endpoint any/c]
                   [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs an unbounded-above @tech{range} with an inclusive lower bound of
 @racket[lower-endpoint]. The constructed range includes every value that is
 greater than or equal to @racket[lower-endpoint] according to @racket[
 comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (at-least 14)
   (range-contains? (at-least 14) 5)
   (range-contains? (at-least 14) 14)
   (range-contains? (at-least 14) 87))}

@defproc[(at-most [upper-endpoint any/c]
                  [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs an unbounded-below @tech{range} with an inclusive upper bound of
 @racket[upper-endpoint]. The constructed range includes every value that is
 less than or equal to @racket[upper-endpoint] according to @racket[
 comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (at-most 14)
   (range-contains? (at-most 14) 5)
   (range-contains? (at-most 14) 14)
   (range-contains? (at-most 14) 87))}

@defproc[(greater-than [lower-endpoint any/c]
                       [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs an unbounded-above @tech{range} with an exclusive lower bound of
 @racket[lower-endpoint]. The constructed range includes every value that is
 greater than @racket[lower-endpoint] according to @racket[comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (greater-than 14)
   (range-contains? (greater-than 14) 5)
   (range-contains? (greater-than 14) 14)
   (range-contains? (greater-than 14) 87))}

@defproc[(less-than [upper-endpoint any/c]
                    [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs an unbounded-below @tech{range} with an exclusive upper bound of
 @racket[upper-endpoint]. The constructed range includes every value that is
 less than @racket[upper-endpoint] according to @racket[comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (less-than 14)
   (range-contains? (less-than 14) 5)
   (range-contains? (less-than 14) 14)
   (range-contains? (less-than 14) 87))}

@defproc[(singleton-range [endpoint any/c]
                          [#:comparator comparator comparator? real<=>])
         range?]{
 Constructs a @tech{singleton range} that contains only values that are
 equivalent to @racket[endpoint], according to @racket[comparator].

 @(examples
   #:eval (make-evaluator) #:once
   (singleton-range 42)
   (range-contains? (singleton-range 42) 42)
   (range-contains? (singleton-range 42) 41)
   (range-contains? (singleton-range 42) 43))}

@section{Querying Ranges}

@defproc[(range-contains? [range range?] [v any/c]) boolean?]{
 Determines whether or not @racket[v] lies within @racket[range] by comparing
 @racket[v] to its bounds, using the range's @tech{comparator}.

 @(examples
   #:eval (make-evaluator) #:once
   (range-contains? (closed-range 3 7) 5)
   (range-contains? (closed-range 3 7) 7)
   (range-contains? (closed-range 3 7) 10)
   (range-contains? (greater-than "apple" #:comparator string<=>) "banana")
   (range-contains? (greater-than "apple" #:comparator string<=>) "aardvark"))}

@defproc[(range-encloses? [range range?] [other-range range?]) boolean?]{
 Determines whether or not @racket[range] @tech{encloses} @racket[other-range].
 One range @deftech{encloses} another range when the first range contains every
 value contained by the second. Both ranges must use the same @tech{comparator},
 or else a contract violation is raised. Every range encloses itself, and empty
 ranges never enclose nonempty ranges.

 @(examples
   #:eval (make-evaluator) #:once
   (range-encloses? (open-range 2 8) (closed-range 4 6))
   (range-encloses? (open-range 2 8) (closed-range 2 6))
   (range-encloses? (open-range 2 8) (at-least 5))
   (range-encloses? (greater-than 2) (at-least 5))
   (eval:error
    (range-encloses? (greater-than 2)
                     (greater-than "apple" #:comparator string<=>))))}

@defproc[(range-connected? [range1 range?] [range2 range?]) boolean?]{
 Determines whether or not there exists a (possibly empty) range that is @tech{
  enclosed} by both @racket[range1] and @racket[range2].

 @(examples
   #:eval (make-evaluator) #:once
   (range-connected? (closed-range 2 7) (open-range 3 8))
   (range-connected? (closed-range 2 5) (open-range 5 8))
   (range-connected? (open-range 2 5) (open-range 5 8)))}

@section{Operations on Ranges}

@defproc[(range-span [range1 range?] [range2 range?]) range?]{
 Returns the smallest range that @tech{encloses} both @racket[range1] and
 @racket[range2]. The ranges need not be connected, but they must use the same
 @tech{comparator}. This operation is commutative, associative, and idempotent.

 @(examples
   #:eval (make-evaluator) #:once
   (range-span (closed-range 2 5) (open-range 8 9))
   (range-span (less-than 4) (singleton-range 6))
   (range-span (open-range 2 8) (at-most 5)))}
