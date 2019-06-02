#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/multiset)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/multiset)
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

@defthing[empty-multiset multiset? #:value (multiset)]{
 The empty @tech{multiset}, which contains no elements.}
