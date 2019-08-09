#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/sequence
                     racket/vector
                     rebellion/base/immutable-string
                     rebellion/base/symbol
                     rebellion/base/variant
                     rebellion/streaming/reducer
                     rebellion/type/record)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/sequence
                   'racket/vector
                   'rebellion/base/immutable-string
                   'rebellion/base/variant
                   'rebellion/streaming/reducer
                   'rebellion/type/record)
    #:private (list 'racket/base)))

@title{Reducers}
@defmodule[rebellion/streaming/reducer]

A @deftech{reducer} is an object that can combine a (possibly infinite) sequence
of elements into a single result value. Reducers are state machines; performing
a reduction involves @emph{starting} the reducer to get an initial state, then
@emph{consuming} elements one at a time to transform the current state into a
new, updated state. When no more elements are available, the reducer's @emph{
 finisher} is called to transform the final state into a result value.
Optionally, a reducer may terminate the reduction early, before the sequence is
fully consumed.

@defproc[(reducer? [v any/c]) boolean?]{
 A predicate for @tech{reducers}.}

@defproc[(reduce [red reducer?] [v any/c] ...) any/c]{
 Reduces @racket[v]s with @racket[red], in left-to-right order.

 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-sum 1 2 3 4 5 6 7 8 9 10)
   (reduce into-product 2 3 5 7 11 13 17 19 23)
   (reduce into-count 'a 'b 'c 'd 'e))}

@defproc[(reduce-all [red reducer?] [seq sequence?]) any/c]{
 Reduces @racket[seq] with @racket[red]. The sequence is iterated lazily, so if
 @racket[red] terminates the reduction early then the sequence will not be fully
 traversed.

 @(examples
   #:eval (make-evaluator) #:once
   (reduce-all into-sum (in-range 1 100))
   (reduce-all into-product (in-range 1 20))
   (reduce-all into-count (in-hash-values (hash 'a 1 'b 2 'c 3 'd 4))))}

@defthing[into-sum reducer?]{
 A @tech{reducer} that reduces a sequence of numbers into their sum with
 @racket[+].

 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-sum 1 2 3)
   (reduce into-sum)
   (reduce-all into-sum (in-range 10000)))}

@defthing[into-product reducer?]{
 A @tech{reducer} that reduces a sequence of numbers into their product with
 @racket[*].

 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-product 2 3 4)
   (reduce into-product)
   (reduce-all into-product (in-range 1 20)))}

@defthing[into-count reducer?]{
 A @tech{reducer} that ignores the specific elements it reduces and returns only
 a count of how many elements were reduced.

 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-count 'a 'b 'c)
   (reduce-all into-count "hello world"))}

@defthing[into-string reducer?]{
 A @tech{reducer} that collects a sequence of individual characters into an
 immutable string.

 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-string #\h #\e #\l #\l #\o)
   (reduce-all into-string (list #\a #\b #\c)))}

@defproc[(join-into-string
          [sep immutable-string?]
          [#:before-first before-first immutable-string? ""]
          [#:before-last before-last immutable-string? sep]
          [#:after-last after-last immutable-string? ""])
         reducer?]{
 Constructs a @tech{reducer} that joins a sequence of immutable strings into a
 single immutable string, in the same manner as @racket[string-join].

 @(examples
   #:eval (make-evaluator) #:once
   (reduce-all (join-into-string " + ")
               (sequence-map number->immutable-string (in-range 1 10)))
   (reduce-all (join-into-string ", " #:before-last ", and ")
               (sequence-map number->immutable-string (in-range 1 10)))
   (reduce-all (join-into-string ", " #:before-first "func(" #:after-last ")")
               (sequence-map number->immutable-string (in-range 1 10))))}

@section{Reducer Constructors}

The full reducer interface is captured by the @racket[make-reducer] constructor,
but this is sufficiently more power than most users should need. Three separate
constructors are provided, each designed for three different categories of
reducers with increasing power and complexity:

@itemlist[
 @item{@racket[make-fold-reducer] constructs @tech{fold reducers}, which can
  express the same reductions as @racket[foldl].}

 @item{@racket[make-effectful-fold-reducer] constructs @tech{effectful fold
   reducers}, which can express folds where a private, potentially mutable
  state value is initialized at the start of reduction and converted into a
  public result value at the end of reduction. Effectful fold reducers are a
  superset of fold reducers.}

 @item{@racket[make-reducer] constructs general @tech{reducers}, with the full
  power of the reduction protocol and the ability to terminate the sequence
  early.}]

@defproc[(make-fold-reducer
          [consumer (-> any/c any/c any/c)]
          [init-state any/c]
          [#:name name (or/c interned-symbol? #f) #f])
         reducer?]{
 Constructs a @deftech{fold reducer}, the simplest type of reducer. A fold
 reducer starts each reduction with an initial state of @racket[init-state] and
 transforms it into a new state by calling @racket[(consumer state element)]
 with each reduced sequence element. When no more elements are available, the
 state is returned as the reduction result.

 @(examples
   #:eval (make-evaluator) #:once
   (define into-reversed-list
     (make-fold-reducer (λ (lst v) (cons v lst)) (list)))
   (reduce-all into-reversed-list (in-range 5 25)))}

@defproc[(make-effectful-fold-reducer
          [consumer (-> any/c any/c any/c)]
          [init-state-maker (-> any/c)]
          [finisher (-> any/c any/c)]
          [#:name name (or/c interned-symbol? #f) #f])
         reducer?]{
 Constructs an @deftech{effectful fold reducer}, which is like a @tech{fold
  reducer} with a private, possibly mutable state. An effectful fold reducer
 starts each reduction by calling @racket[(init-state-maker)] to construct an
 initial state. Elements are consumed in the same way as @tech{fold reducers} by
 calling @racket[(consumer state element)]. When no more elements are available,
 @racket[(finisher state)] is called to determine the convert the final state
 into the reduction result.

 @(examples
   #:eval (make-evaluator) #:once
   (define into-list
     (make-effectful-fold-reducer (λ (lst v) (cons v lst)) list reverse))
   (reduce-all into-list (in-range 5 25)))}

@defproc[(make-reducer
          [#:starter starter
           (-> (variant/c #:consume any/c #:early-finish any/c))]
          [#:consumer consumer
           (-> any/c any/c (variant/c #:consume any/c #:early-finish any/c))]
          [#:finisher finisher (-> any/c any/c)]
          [#:early-finisher early-finisher (-> any/c any/c)]
          [#:name name (or/c interned-symbol? #f) #f])
         reducer?]{
 Constructs a @tech{reducer} that reduces sequences by following the following
 steps, known as the @deftech{reduction protocol}:

 @itemlist[
 @item{Start the reduction by calling @racket[(starter)] to create the initial
   reduction state, which must be a @tech{variant} tagged as either @racket[
 #:consume] or @racket[#:early-finish].}

 @item{If the current state is tagged as @racket[#:consume], and the sequence is
   not empty, call @racket[(consumer (variant-value state) element)] with the
   next sequence element to get the updated reduction state. Repeat this step
   until either no more elements are available or until the reduction state is
   tagged as @racket[#:early-finish].}

 @item{If the current state is tagged as @racket[#:early-finish], call @racket[
 (early-finisher (variant-value state))] to determine the @deftech{reduction
    result}. Otherwise, call @racket[(finisher (variant-value state))] to get
   the reduction result.}]

 @(examples
   #:eval (make-evaluator) #:once
   (define-record-type state (vector position))

   (define into-small-immutable-vector
     (make-reducer
      #:starter
      (λ ()
        (variant #:consume
                 (state #:vector (make-vector 10 #f)
                        #:position 0)))
      #:consumer
      (λ (st v)
        (define i (state-position st))
        (define vec (state-vector st))
        (vector-set! vec i v)
        (define i* (add1 i))
        (if (< i* 10)
            (variant #:consume (state #:vector vec #:position i*))
            (variant #:early-finish vec)))
      #:finisher
      (λ (st)
        (define vec (state-vector st))
        (define i (state-position st))
        (vector->immutable-vector (vector-copy vec 0 i)))
      #:early-finisher vector->immutable-vector))

   (reduce into-small-immutable-vector 1 2 3)
   (reduce-all into-small-immutable-vector (in-naturals)))}

@section{Reducer Operators}

@defproc[(reducer-map [red reducer?]
                      [#:domain f (-> any/c any/c) values]
                      [#:range g (-> any/c any/c) values])
         reducer?]{
 Wraps @racket[red] to apply @racket[f] to each sequence element and to apply
 @racket[g] to its reduction result. Both @racket[f] and @racket[g] default to
 @racket[values].

 @(examples
   #:eval (make-evaluator) #:once
   (define into-total-letters
     (reducer-map into-sum #:domain string-length))
   (reduce into-total-letters "the" "quick" "brown" "fox")
   
   (define stringly-typed-into-sum
     (reducer-map into-sum
                  #:domain string->number
                  #:range number->string))
   (reduce stringly-typed-into-sum "12" "5" "42" "17"))}

@defproc[(reducer-filter [red reducer?] [pred predicate/c]) reducer?]{
 Wraps @racket[red] to only reduce sequence elements for which @racket[pred]
 returns @racket[#t], and ignore elements completely when @racket[pred] returns
 @racket[#f].

 @(examples
   #:eval (make-evaluator) #:once
   (define numbers-into-sum (reducer-filter into-sum number?))
   (reduce numbers-into-sum 1 'a 2 3 'b 'c 'd 4 'e 5))}

@section{Iteration and Comprehension with Reducers}

@defform[(for/reducer reducer-expr (for-clause ...) body-or-break ... body)
         #:contracts ([reducer-expr reducer?])]{
 Iterates like @racket[for], but the sequence of iterated @racket[body] results
 is reduced with @racket[reducer-expr].

 @(examples
   #:eval (make-evaluator) #:once
   (for/reducer into-sum
     ([char (in-string "aaa0aa00a0aa")])
     (if (char-alphabetic? char)
         1
         -1)))}

@defform[(for*/reducer reducer-expr (for-clause ...) body-or-break ... body)
         #:contracts ([reducer-expr reducer?])]{
 Iterates like @racket[for*], but the sequence of iterated @racket[body] results
 is reduced with @racket[reducer-expr].}

@defproc[(make-reducer-based-for-comprehensions [reducer-expression syntax?])
         (values (-> syntax? syntax?)
                 (-> syntax? syntax?))]{
 Returns two @tech{syntax transformers} suitable for use with @racket[
 define-syntaxes] that implement two @racket[for]-like macros. The returned
 macros use @racket[reducer-expression] to iterate like @racket[for/reducer]
 and @racket[for*/reducer], respectively. Provided at phase 1.

 In order to prevent confusion over how many times @racket[reducer-expression]
 is expanded and evaluated, strongly prefer using a single identifier for
 @racket[reducer-expression] instead of an expression using
 @racket[reducer-map], @racket[make-fold-reducer], etc.

 @(examples
   #:eval (make-evaluator) #:once
   (require (for-syntax racket/base))
   (define-syntaxes (for/sum for*/sum)
     (make-reducer-based-for-comprehensions #'into-sum))
   (for/sum ([str (in-list (list "apple" "orange" "banana" "grapefruit"))])
     (string-length str)))}
