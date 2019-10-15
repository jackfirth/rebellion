#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/comparator
                     rebellion/base/symbol
                     rebellion/type/record)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/comparator
                   'rebellion/type/record)
    #:private (list 'racket/base)))

@title{Comparators}
@defmodule[rebellion/base/comparator]

A @deftech{comparator} is an object that compares two values and determines
which (if either) is greater.

@defproc[(comparator? [v any/c]) boolean?]{
 A predicate for @tech{comparators}.}

@defproc[(compare [comparator comparator?] [left any/c] [right any/c])
         comparison?]{
 Returns whether @racket[left] is less than, greater than, or equivalent to
 @racket[right].

 @(examples
   #:eval (make-evaluator) #:once
   (compare real<=> 5 8)
   (compare string<=> "foo" "bar"))}

@section{Constructing Comparators}

@defproc[(comparator-map [comparator comparator?]
                         [f (-> any/c any/c)]
                         [#:name name (or/c interned-symbol? #f) #f])
         comparator?]{
 Wraps @racket[comparator] as a @tech{comparator} that first calls @racket[f] on
 both of its inputs before comparing them.

 @(examples
   #:eval (make-evaluator) #:once
   (define-record-type circle (color radius))
   (define circle<=> (comparator-map real<=> circle-radius))
   (compare circle<=>
            (circle #:color 'green #:radius 5)
            (circle #:color 'blue #:radius 8)))}

@defproc[(make-comparator [function (-> any/c any/c comparison?)]
                          [#:name name (or/c interned-symbol? #f) #f])
         comparator?]{
 Constructs a @tech{comparator} named @racket[name] that compares values by
 calling @racket[function]. Most users should use @racket[comparator-map] to
 wrap an existing comparator instead of constructing one directly.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define symbol<=>
      (make-comparator
       (λ (left right)
         (cond [(symbol<? left right) lesser]
               [(equal? left right) equivalent]
               [else greater]))
       #:name 'symbol<=>)))
   (compare symbol<=> 'apple 'banana)
   (compare symbol<=> 'apple 'aardvark))}

@defproc[(comparator-reverse [comparator comparator?]) comparator?]{
 Reverses @racket[comparator], returning a comparator that considers one value
 greater than another when @racket[comparator] would consider it lesser and
 vice-versa.

 @(examples
   #:eval (make-evaluator) #:once
   (compare real<=> 2 5)
   (compare (comparator-reverse real<=>) 2 5))}

@section{Predefined Comparators}

@defthing[real<=> comparator?]{
 A @tech{comparator} that compares real numbers.}

@defthing[string<=> comparator?]{
 A @tech{comparator} that lexicographically compares immutable strings. Mutable
 strings are disallowed, to prevent clients from concurrently mutating a string
 while it's being compared.}

@section{Comparison Constants}

@defproc[(comparison? [v any/c]) boolean?]{
 A predicate for comparison constants.}

@defthing[lesser comparison?]{
 A comparison constant indicating that the left value of a comparison is smaller
 than the right value.}

@defthing[greater comparison?]{
 A comparison constant indicating that the left value of a comparison is greater
 than the right value.}

@defthing[equivalent comparison?]{
 A comparison constant indicating that the left value of a comparison is
 equivalent to the right value. Note that equivalent values may not be @racket[
 equal?], depending on the @tech{comparator} used.}
