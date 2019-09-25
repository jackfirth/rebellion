#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/multiset
                     rebellion/streaming/reducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/multiset
                   'rebellion/streaming/reducer)
    #:private (list 'racket/base)))

@title{Multisets}
@defmodule[rebellion/collection/multiset]

A @deftech{multiset} is an unordered collection, like a @tech{set}, except it
can contain duplicate elements. Elements are always compared with @racket[
 equal?].

@defproc[(multiset? [v any/c]) boolean?]{
 A predicate for @tech{multisets}.}

@defproc[(multiset [v any/c] ...) multiset?]{
 Constructs a @tech{multiset} containing the given @racket[v]s.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset 'apple 'orange 'banana)
   (multiset 'apple 'orange 'orange 'banana)
   (multiset))}

@defproc[(multiset-size [set multiset?]) natural?]{
 Returns the total number of elements in @racket[set], including duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (define set (multiset 5 8 8 8))
   (multiset-size set))}

@defproc[(multiset-add [set multiset?] [v any/c]) multiset?]{
 Adds @racket[v] to @racket[set], returning an updated @tech{multiset}. The
 original @racket[set] is not mutated.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-add (multiset 'apple 'orange 'banana) 'grape)
   (multiset-add (multiset 'apple 'orange 'banana) 'orange))}

@defproc[(multiset-add-all [set multiset?] [seq (sequence/c any/c)]) multiset?]{
 Adds @racket[seq] elements into @racket[set], returning an updated
 @tech{multiset}. The original @racket[set] is not mutated.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-add-all (multiset 1 1 2 3) (in-range 0 5))
   (multiset-add-all (multiset 1 2 3) (multiset 1 2 3)))}

@defproc[(multiset-remove-once [set multiset?] [v any/c]) multiset?]{
 Removes a single @racket[v] from @racket[set], returning an updated
 @tech{multiset}. The original @racket[set] is not mutated.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-remove-once (multiset 'apple 'orange 'banana) 'grape)
   (multiset-remove-once (multiset 'apple 'orange 'banana) 'orange)
   (multiset-remove-once (multiset 'apple 'apple 'orange 'banana) 'apple))}

@defproc[(multiset-contains? [set multiset?] [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[set] contains @racket[v], @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (define set (multiset 'apple 'orange 'orange))
   (multiset-contains? set 'apple)
   (multiset-contains? set 'orange)
   (multiset-contains? set 42))}

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

@defproc[(multiset-unique-elements [set multiset?]) immutable-set?]{
 Removes all duplicate elements from @racket[set], returning the resulting set.

 @(examples
   #:eval (make-evaluator) #:once
   (multiset-unique-elements (multiset 5 8 8 8 13 13)))}

@section{Multiset Iteration and Comprehension}

@defproc[(in-multiset [set multiset?]) sequence?]{
 Returns a @tech{sequence} that iterates over the elements of @racket[set],
 including duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (define set (multiset 5 8 8 8 5 3))
   (for ([v (in-multiset set)])
     (displayln v)))}

@defthing[into-multiset reducer?]{
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

@defproc[(list->multiset [lst list?]) multiset?]{
 Returns a @tech{multiset} containing the elements of @racket[lst], including
 duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (list->multiset (list 'a 'a 'b 'c 'c 'c 'd)))}

@defproc[(sequence->multiset [seq sequence?]) multiset?]{
 Returns a @tech{multiset} containing the elements of @racket[seq], including
 duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (sequence->multiset (in-range 5)))}

@defthing[empty-multiset multiset? #:value (multiset)]{
 The empty @tech{multiset}, which contains no elements.}
