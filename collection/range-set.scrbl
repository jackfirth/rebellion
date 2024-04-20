#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     racket/set
                     rebellion/base/comparator
                     rebellion/base/option
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


A @deftech{range set} is a sorted @tech{collection} of nonempty, disconnected @tech{ranges}. All
ranges in the same range set must use the same @tech{comparator}. Two immutable range sets are
@racket[equal?] if they contain the same ranges. Two mutable range sets are @racket[equal?] if they
will @emph{always} contain the same ranges, meaning that they share the same mutable state. This is
not necessarily the same as being @racket[eq?], as some range sets may be views of others.

All range sets are @tech/reference{sequences}. When iterated, a range set traverses its ranges in
ascending order as defined by the comparator shared by those ranges. To traverse a range set in
descending order, use @racket[in-range-set] with @racket[#:descending?] set to true.


@defproc[(range-set? [v any/c]) boolean?]{
 A predicate for @tech{range sets}. Includes mutable, immutable, and @tech{unmodifiable} range sets.}


@defproc[(immutable-range-set? [v any/c]) boolean?]{
 A predicate for immutable @tech{range sets}. Implies @racket[range-set?].}


@defproc[(mutable-range-set? [v any/c]) boolean?]{
 A predicate for mutable @tech{range sets}. Implies @racket[range-set?].}


@section{Constructing Range Sets}


@defproc*[([(range-set
             [range nonempty-range?] ...
             [#:comparator comparator comparator?])
            immutable-range-set?]
           [(range-set
             [first-range nonempty-range?]
             [range nonempty-range?] ...
             [#:comparator comparator comparator? (range-comparator first-range)])
            immutable-range-set?])]{
 Constructs an immutable @tech{range set} containing the given @racket[range]s. If at least one
 @racket[range] is provided, @racket[comparator] may be omitted and defaults to the comparator of the
 first range. Overlapping ranges are disallowed, but adjacent ranges are accepted and are merged
 together. All ranges must use @racket[comparator] as their endpoint comparators or a contract
 exception is raised.

 @(examples
   #:eval (make-evaluator) #:once
   (range-set (closed-open-range 2 5))
   (range-set (closed-open-range 4 8) (closed-open-range 0 4))
   (range-set (greater-than-range 20) (less-than-range 5) (closed-range 10 15))
   (range-set (closed-open-range 2 5) #:comparator real<=>))}


@defproc[(make-mutable-range-set
          [initial-ranges (sequence/c range?) '()]
          [#:comparator comparator comparator?])
         mutable-range-set?]{
 Constructs a new mutable @tech{range set} containing @racket[initial-ranges] (which defaults to the
 empty list). All ranges must use @racket[comparator] as their endpoint comparators or a contract
 exception is raised. As in the @racket[range-set] constructor, overlapping ranges are disallowed.

 @(examples
   #:eval (make-evaluator) #:once
   (make-mutable-range-set #:comparator real<=>)
   (make-mutable-range-set (list (closed-range 2 5) (open-range 10 20))
                           #:comparator real<=>))}


@defproc[(sequence->range-set
          [ranges (sequence/c nonempty-range?)]
          [#:comparator comparator comparator?])
         immutable-range-set?]{
 Constructs an immutable @tech{range set} from the given @racket[ranges] (which must use
 @racket[comparator].) As in the @racket[range-set] consturctor, overlapping ranges are disallowed.

 @(examples
   #:eval (make-evaluator) #:once
   (sequence->range-set
    (list (closed-range 1 3) (closed-range 10 12) (closed-range 5 6))
    #:comparator real<=>))}


@defform[(for/range-set #:comparator comparator (for-clause ...)
           body-or-break ...
           body)]{
 Like @racket[for], but collects the iterated ranges into an immutable @tech{range set}.

 @(examples
   #:eval (make-evaluator) #:once
   (for/range-set #:comparator real<=> ([char (in-string "abcxyz")])
     (define codepoint (char->integer char))
     (closed-open-range codepoint (add1 codepoint))))}


@defform[(for*/range-set #:comparator comparator (for-clause ...)
           body-or-break ...
           body)]{
 Like @racket[for*], but collects the iterated ranges into an immutable @tech{range set}.

 @(examples
   #:eval (make-evaluator) #:once
   (for*/range-set #:comparator real<=>
     ([str (in-list (list "abc" "tuv" "xyz" "qrs"))]
      [char (in-string str)])
     (define codepoint (char->integer char))
     (closed-open-range codepoint (add1 codepoint))))}


@defproc[(into-range-set [comparator comparator?]) (reducer/c nonempty-range? immutable-range-set?)]{
 Constructs a @tech{reducer} that reduces a sequence of nonempty ranges (which must use
 @racket[comparator]) into a @tech{range set}. As in the @racket[range-set] constructor, overlapping
 ranges are disallowed.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list (closed-range 1 3) (closed-range 10 12) (closed-range 5 6))
              #:into (into-range-set real<=>)))}


@section{Iterating Range Sets}


@defproc[(in-range-set [range-set range-set?] [#:descending? descending? boolean? #false])
         (sequence/c nonempty-range?)]{
 Returns a @tech/reference{sequence} iterating through the ranges in @racket[range-set] in ascending
 order. Note that @tech{range sets} are already sequences, but this form may yield better performance
 when used directly within a @racket[for] clause.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 1 3) (closed-range 10 12) (closed-range 5 6))))

   (for ([r (in-range-set ranges)])
     (displayln r))

   (for ([r (in-range-set ranges #:descending? #true)])
     (displayln r)))}


@section{Querying Range Sets}


@defproc[(range-set-size [ranges range-set?]) natural?]{
 Returns the number of ranges in @racket[ranges]

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-size (range-set (closed-range 3 5) (closed-range 8 14))))}


@defproc[(range-set-comparator [ranges range-set?]) comparator?]{
 Returns the comparator that @racket[ranges] uses to compare elements to its contained ranges.}


@defproc[(range-set-empty? [ranges range-set?]) boolean?]{
 Returns true if @racket[ranges] is empty, returns false otherwise.}


@defproc[(range-set-contains? [ranges range-set?] [element any/c]) boolean?]{
 Determines if any range in @racket[ranges] contains @racket[element].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 9 10))))
   (range-set-contains? ranges 2)
   (range-set-contains? ranges 7)
   (range-set-contains? ranges 10))}


@defproc[(range-set-contains-all? [ranges range-set?] [elements (sequence/c any/c)]) boolean?]{
 Determines if @racket[ranges] contains every value in @racket[vs]. If @racket[elements] is empty,
 returns true.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 9 10))))

   (range-set-contains-all? ranges (list 2 3 4 5 9 10))
   (range-set-contains-all? ranges (list 4 7))
   (range-set-contains-all? ranges (list)))}


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
 Determines if @racket[ranges] encloses every range in @racket[other-ranges]. If @racket[other-ranges]
 is empty, returns true.

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-encloses-all? (range-set (closed-range 2 5) (closed-range 7 10))
                            (range-set (closed-range 3 4) (closed-range 8 9))))}


@defproc[(range-set-intersects? [ranges range-set?] [other-range range?]) boolean?]{
 Determines if any range in @racket[ranges] intersects with @racket[other-range].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 9 10))))

   (range-set-intersects? ranges (closed-range 4 8))
   (range-set-intersects? ranges (closed-range 6 8)))}


@defproc[(range-set-range-containing [ranges range-set?]
                                     [element any/c]
                                     [failure-result failure-result/c (λ () (raise ...))])
         any/c]{
 Returns the range in @racket[ranges] that contains @racket[element]. If no range contains
 @racket[element], then @racket[failure-result] determines the result: if it's a procedure it's called
 with no arguments to produce the result, if it's not a procedure it's returned directly.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 9 10))))
   
   (range-set-range-containing ranges 4)
   (range-set-range-containing ranges 9)
   (eval:error (range-set-range-containing ranges 7))
   (range-set-range-containing ranges 7 #false))}


@defproc[(range-set-range-containing-or-absent [ranges range-set?] [element any/c])
         (option/c range?)]{
 Returns the range in @racket[ranges] that contains @racket[element], wrapped in a @racket[present]
 option. If no range contains @racket[element], then @racket[absent] is returned.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 9 10))))
   
   (range-set-range-containing-or-absent ranges 4)
   (range-set-range-containing-or-absent ranges 9)
   (range-set-range-containing-or-absent ranges 7))}


@defproc[(range-set-span [ranges range-set?] [empty-result failure-result/c (λ () (raise ...))])
         any/c]{
 Returns the span of @racket[ranges], which is the smallest range that encloses every range in the
 set. If @racket[ranges] is empty, then @racket[empty-result] determines the result: if it's a
 procedure it's called with no arguments to produce the result, if it's not a procedure it's returned
 directly.

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-span (range-set (closed-range 2 5) (open-range 9 10)))
   (eval:error (range-set-span (range-set #:comparator real<=>)))
   (range-set-span (range-set #:comparator real<=>) "empty range set!"))}


@defproc[(range-set-span-or-absent [ranges range-set?]) (option/c range?)]{
 Returns the span of @racket[ranges], which is the smallest range that encloses every range in the
 set. The returned span is wrapped in a @racket[present] option. If @racket[ranges] is empty, then
 @racket[absent] is returned.

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-span-or-absent (range-set (closed-range 2 5) (open-range 9 10)))
   (range-set-span-or-absent (range-set #:comparator real<=>)))}


@section{Range Set Views}


@defproc[(range-subset [ranges range-set?] [subset-range range?]) range-set?]{

 Returns a range set containing the ranges in @racket[ranges] that are enclosed by
 @racket[subset-range].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 7 10))))
   (range-subset ranges (less-than-range 4)))}


@section{Modifying Range Sets}


@defproc[(range-set-add [ranges immutable-range-set?] [new-range range?]) immutable-range-set?]{

 Functionally adds @racket[new-range] to @racket[ranges] by returning a new range set containing
 all of the ranges in @racket[ranges] and @racket[new-range]. Overlapping and adjacent ranges are
 coalesced. The input range set is not modified.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (range-set (closed-range 2 5) (closed-range 7 10))))
   (range-set-add ranges (closed-range 0 3))
   (range-set-add ranges (closed-range 6 12))
   (range-set-add ranges (closed-range 4 8)))}


@defproc[(range-set-add! [ranges mutable-range-set?] [new-range range?]) void?]{

 Adds @racket[new-range] to @racket[ranges]. Overlapping and adjacent ranges are coalesced.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (make-mutable-range-set #:comparator real<=>))
    (range-set-add! ranges (closed-range 2 5))
    (range-set-add! ranges (closed-range 7 10)))

   ranges

   (eval:no-prompt
    (range-set-add! ranges (closed-range 4 8)))
   
   ranges)}


@defproc[(range-set-add-all [ranges immutable-range-set?] [new-ranges (sequence/c range?)])
         immutable-range-set?]{

 Functionally adds every range in @racket[new-ranges] to @racket[ranges] by returning a new range set
 containing all of the ranges in @racket[ranges] and all of the ranges in @racket[new-ranges].
 Overlapping and adjacent ranges are coalesced. The input range set is not modified. This function is
 equivalent to looping over @racket[new-ranges] with @racket[range-set-add].

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-add-all (range-set (closed-range 2 5) (open-range 10 12))
                      (range-set (open-range 3 8) (closed-range 15 20))))}


@defproc[(range-set-add-all! [ranges mutable-range-set?] [new-ranges (sequence/c range?)]) void?]{

 Adds every range in @racket[new-ranges] to @racket[ranges]. Overlapping and adjacent ranges are
 coalesced. This function is equivalent to looping over @racket[new-ranges] with
 @racket[range-set-add!].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (make-mutable-range-set #:comparator real<=>)))

   (eval:no-prompt
    (range-set-add-all! ranges (list (closed-range 2 5) (open-range 10 12)))
    (range-set-add-all! ranges (list (open-range 3 8) (closed-range 15 20))))

   ranges)}


@defproc[(range-set-remove [ranges immutable-range-set?] [range-to-remove range?])
         immutable-range-set?]{

 Functionally removes @racket[range-to-remove] from @racket[ranges] by returning a new range set
 containing all of the ranges in @racket[ranges] except any ranges enclosed by
 @racket[range-to-remove]. If a range only partially intersects @racket[range-to-remove], only the
 intersecting portion is removed in the returned range set. The input range set is not modified.

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-remove (range-set (closed-range 1 5) (closed-range 8 10))
                     (closed-range 3 12)))}


@defproc[(range-set-remove! [ranges mutable-range-set?] [range-to-remove range?]) void?]{

 Removes @racket[range-to-remove] from @racket[ranges] by removing all of the ranges in
 @racket[ranges] that are enclosed by @racket[range-to-remove]. If a range only partially intersects
 @racket[range-to-remove], only the intersecting portion is removed from the range set.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (make-mutable-range-set #:comparator real<=>))
    (range-set-add-all! ranges (list (closed-range 1 5) (closed-range 8 10))))

   ranges

   (eval:no-prompt
    (range-set-remove! ranges (closed-range 3 12)))

   ranges)}


@defproc[(range-set-remove-all [ranges immutable-range-set?] [ranges-to-remove (sequence/c range?)])
         immutable-range-set?]{

 Functionally removes every range in @racket[ranges-to-remove] from @racket[ranges] by returning a new
 range set containing all of the ranges in @racket[ranges] except any ranges enclosed by a range in
 @racket[ranges-to-remove]. If a range only partially intersects @racket[range-to-remove], only the
 intersecting portion is removed in the returned range set. The input range set is not modified. This
 function is equivalent to looping over @racket[ranges-to-remove] with @racket[range-set-remove].

 @(examples
   #:eval (make-evaluator) #:once
   (range-set-remove-all (range-set (closed-range 1 5) (closed-range 8 10))
                         (range-set (open-range 3 7) (closed-range 9 15))))}


@defproc[(range-set-remove-all! [ranges mutable-range-set?] [ranges-to-remove (sequence/c range?)])
         void?]{

 Removes every range in @racket[ranges-to-remove] from @racket[ranges] by removing all of the ranges
 in @racket[ranges] that are enclosed by any range in @racket[ranges-to-remove]. If a range only
 partially intersects a range in @racket[ranges-to-remove], only the intersecting portion is removed
 from the range set. This function is equivalent to looping over @racket[ranges-to-remove] with
 @racket[range-set-remove!].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define ranges (make-mutable-range-set #:comparator real<=>))
    (range-set-add-all! ranges (list (closed-range 1 5) (closed-range 8 10))))

   ranges

   (eval:no-prompt
    (range-set-remove-all! ranges (list (open-range 3 7) (closed-range 9 15))))

   ranges)}


@defproc[(range-set-clear! [ranges mutable-range-set?]) void?]
