#lang scribble/manual

@(require (for-label racket/base
                     racket/bool
                     racket/contract/base
                     rebellion/base/comparator
                     rebellion/base/symbol
                     rebellion/collection/hash
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer
                     rebellion/type/record)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/comparator
                   'rebellion/streaming/reducer
                   'rebellion/streaming/transducer
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

@section{Comparator Chaperones and Impersonators}

@defproc[(comparator-impersonate
          [comparator comparator?]
          [#:operand-guard operand-guard (or/c (-> any/c any/c #f)) #f]
          [#:properties properties
           (hash/c impersonator-property? any/c #:immutable #t)
           empty-hash]
          [#:comparison-marks marks immutable-hash? empty-hash]
          [#:chaperone? chaperone? boolean? (false? operand-guard)])
         comparator?]{
 Returns an @tech/reference{impersonator} of @racket[comparator]. Whenever the
 impersonator comparator is used to compare two values, if @racket[
 operand-guard] is not false, it is applied once to each value. The application
 of the comparator's comparison function to those two values is wrapped in the
 @tech/reference{continuation marks} given by @racket[marks]. Additionally, the
 returned impersonator includes an @tech/reference{impersonator property} for
 each property and value entry within @racket[properties]. The returned
 impersonator is @racket[equal?] to @racket[comparator].

 If @racket[chaperone?] is true, the returned impersonator is a @tech/reference{
  chaperone}. In that case, @racket[operand-guard] must always return a value
 equal to the one it is given. Furthermore, any impersonators returned from
 @racket[operand-guard] must be chaperones.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define printing-real<=>
      (comparator-impersonate real<=>
                              #:operand-guard (λ (x) (printf "Got ~a\n" x) x)
                              #:chaperone? #t)))

   (compare printing-real<=> 4 8)
   (chaperone-of? printing-real<=> real<=>))}
