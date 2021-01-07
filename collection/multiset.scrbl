#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     racket/set
                     rebellion/collection/multiset
                     rebellion/streaming/reducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/multiset
                   'rebellion/streaming/reducer)
    #:private (list 'racket/base)))

@title{Multisets}
@defmodule[rebellion/collection/multiset]

A @deftech{multiset} is an unordered collection, like a @tech/reference{set},
except it can contain duplicate elements. Elements are always compared with
@racket[equal?].

@defproc[(multiset? [v any/c]) boolean?]{
 A predicate for @tech{multisets}.}

@defproc[(multiset [v any/c] ...) multiset?]{
 Constructs a @tech{multiset} containing the given @racket[v]s.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset 'apple 'orange 'banana)
   (multiset 'apple 'orange 'orange 'banana)
   (multiset))}

@defthing[empty-multiset multiset? #:value (multiset)]{
 The empty @tech{multiset}, which contains no elements.}

@section{Querying Multisets}

@defproc[(multiset-size [set multiset?]) natural?]{
 Returns the total number of elements in @racket[set], including duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (define set (multiset 5 8 8 8))
   (multiset-size set))}

@defproc[(multiset-frequency [set multiset?] [v any/c]) natural?]{
 Returns the number of times that @racket[set] contains @racket[v].

 @(examples
   #:eval (make-evaluator) #:once
   (define set (multiset 5 8 8 8))
   (multiset-frequency set 5)
   (multiset-frequency set 8)
   (multiset-frequency set 'apple))}

@defproc[(multiset-frequencies [set multiset?])
         (immutable-hash/c any/c exact-positive-integer?)]{
 Returns a hash mapping the unique elements of @racket[set] to the number of
 times they occur in @racket[set].

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-frequencies
    (multiset 'red 'red 'red 'blue 'green 'green 'green 'green)))}

@defproc[(multiset-contains? [set multiset?] [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[set] contains @racket[v], @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (define set (multiset 'apple 'orange 'orange))
   (multiset-contains? set 'apple)
   (multiset-contains? set 'orange)
   (multiset-contains? set 42))}

@defproc[(multiset-unique-elements [set multiset?]) set?]{
 Removes all duplicate elements from @racket[set], returning the resulting set.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-unique-elements (multiset 5 8 8 8 13 13)))}

@section{Persistently Updating Multisets}

Multisets are always immutable. The following update operations return new
multisets and leave the input multiset unchanged. However, multisets are
implemented with an efficient persistent data structure that allows the modified
multisets to share their structure with the input multiset. The precise
performance characteristics of these operations are not specified at this time,
but their running times are all sublinear in the number of distinct elements in
the modified multiset.

@defproc[(multiset-add [set multiset?]
                       [element any/c]
                       [#:copies num-copies natural? 1])
         multiset?]{
 Adds @racket[element] to @racket[set], returning an updated @tech{multiset}. If
 @racket[copies] is specified, then @racket[element] is added @racket[
 num-copies] times instead of only once.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-add (multiset 'apple 'orange 'banana) 'grape)
   (multiset-add (multiset 'apple 'orange 'banana) 'orange)
   (multiset-add (multiset 'apple 'orange 'banana) 'orange #:copies 5))}

@defproc[(multiset-add-all [set multiset?] [seq (sequence/c any/c)]) multiset?]{
 Adds @racket[seq] elements into @racket[set], returning an updated
 @tech{multiset}. The original @racket[set] is not mutated.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-add-all (multiset 1 1 2 3) (in-range 0 5))
   (multiset-add-all (multiset 1 2 3) (multiset 1 2 3)))}

@defproc[(multiset-remove [set multiset?]
                          [element any/c]
                          [#:copies num-copies (or/c natural? +inf.0) 1])
         multiset?]{
 Removes a single @racket[element] from @racket[set], returning an updated
 @tech{multiset}. If @racket[num-copies] is specified then instead that many
 copies of @racket[element] are removed from @racket[set], removing all
 occurrences if @racket[num-copies] is @racket[+inf.0] or if @racket[set]
 contains fewer occurrences of @racket[element] than @racket[num-copies].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define set (multiset 'blue 'blue 'red 'red 'red 'green)))
   (multiset-remove set 'red)
   (multiset-remove set 'red #:copies 2)
   (multiset-remove set 'red #:copies 5)
   (multiset-remove set 'blue #:copies +inf.0)
   (multiset-remove set 'orange))}

@defproc[(multiset-set-frequency [set multiset?]
                                 [element any/c]
                                 [frequency natural?])
         multiset?]{
 Adds or removes copies of @racket[element] to or from @racket[set] until it
 occurs exactly @racket[frequency] times in @racket[set]. If @racket[frequency]
 is zero, this is equivalent to removing all copies of @racket[element] from
 @racket[set].

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-set-frequency (multiset 'red 'red 'blue 'green) 'blue 4)
   (multiset-set-frequency (multiset 'red 'red 'blue 'green) 'blue 0))}

@section{Multiset Iteration and Comprehension}

@defproc[(in-multiset [set multiset?]) sequence?]{
 Returns a @tech/reference{sequence} that iterates over the elements of @racket[
 set], including duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (define set (multiset 5 8 8 8 5 3))
   (for ([v (in-multiset set)])
     (displayln v)))}

@defthing[into-multiset (reducer/c any/c multiset?)]{
 A @tech{reducer} that collects elements into a @tech{multiset}.

 @(examples
   #:eval (make-evaluator) #:once
   (reduce-all into-multiset (in-string "happy birthday!")))}

@defform[(for/multiset (for-clause ...) body-or-break ... body)]{
 Like @racket[for], but collects the iterated values into a @tech{multiset}.

 @(examples
   #:eval (make-evaluator) #:once
   (for/multiset ([char (in-string "Hello World")])
     (cond [(char-whitespace? char) 'whitespace]
           [(char-upper-case? char) 'uppercase-letter]
           [else 'lowercase-letter])))}

@defform[(for*/multiset (for-clause ...) body-or-break ... body)]{
 Like @racket[for*], but collects the iterated values into a @tech{multiset}.

 @(examples
   #:eval (make-evaluator) #:once
   (for*/multiset ([str (in-list (list "the" "quick" "brown" "fox"))]
                   [char (in-string str)])
     char))}

@section{Multiset Conversions}

@defproc[(multiset->list [set multiset?]) list?]{
 Returns a list of all the elements of @racket[set]. Duplicate elements are
 adjacent in the returned list, but the order of unequal elements is unspecified
 and should not be relied upon.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset->list (multiset 'a 'a 'b 'c 'c 'c 'd)))}

@defproc[(sequence->multiset [seq sequence?]) multiset?]{
 Returns a @tech{multiset} containing the elements of @racket[seq], including
 duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (sequence->multiset (in-range 5)))}
