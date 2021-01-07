#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/equivalence-relation
                     rebellion/base/symbol)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/equivalence-relation)
    #:private (list 'racket/base)))

@title{Equivalence Relations}
@defmodule[rebellion/base/equivalence-relation]

An @deftech{equivalence relation} is a boolean-returning function of two
arguments that returns true when two values are equivalent, according to some
notion of equivalence. The details of what counts as equivalent vary based on
the equivalence relation used, but all equivalence relations should obey the
following laws:

@itemlist[
 @item{@emph{Reflexive property} --- every value should always be equivalent to
  itself, meaning @racket[(equivalence-relation-holds? _rel _x _x)] is always
  true.}

 @item{@emph{Symmetric property} --- if two values are equivalent, then the
  order in which they are given to the function should not matter, meaning
  @racket[(equivalence-relation-holds? _rel _x _y)] implies @racket[
 (equivalence-relation-holds? _rel _y _x)].}

 @item{@emph{Transitive property} --- if @racket[_x] is equivalent to @racket[
 _y], and @racket[_y] is equivalent to @racket[_z], then @racket[_x] must be
  equivalent to @racket[_z].}]

@defproc[(equivalence-relation? [v any/c]) boolean?]{
 A predicate for @tech{equivalence relations}.}

@defproc[(equivalence-relation-holds? [relation equivalence-relation?]
                                      [x any/c]
                                      [y any/c])
         boolean?]{
 Tests whether @racket[x] and @racket[y] are equivalent, according to @racket[
 relation].

 @(examples
   #:eval (make-evaluator) #:once
   (equivalence-relation-holds? natural-equality 1 1)
   (equivalence-relation-holds? natural-equality 1 2))}

@defproc[(make-equivalence-relation [function (-> any/c any/c boolean?)]
                                    [#:name name (or/c interned-symbol? #f) #f])
         equivalence-relation?]{
 Constructs an equivalence relation that tests whether two values are equivalent
 by calling @racket[function].}

@defproc[(equivalence-relation-function [relation equivalence-relation?])
         (-> any/c any/c boolean?)]{
 Extracts the function from @racket[relation] that implements the behavior of
 calling @racket[equivalence-relation-holds?] with @racket[relation].}

@defthing[natural-equality equivalence-relation?]{
 An @tech{equivalence relation} that considers two values equivalent when they
 are @racket[equal?].}

@defthing[object-identity-equality equivalence-relation?]{
 An @tech{equivalence relation} that considers two values equivalent when they
 refer to the same object, i.e. when they are @racket[eq?].}

@defthing[numeric-equality equivalence-relation?]{
 An @tech{equivalence relation} on numbers that is consistent with the IEEE
 standard for numeric equality, as implemented by @racket[=]. Beware that this
 breaks the reflexive property of equivalence relations, as IEEE specifies that
 @racket[+nan.0] (and @racket[-nan.0]) must not be equal to itself. If this
 relation is restricted to numbers other than @racket[+nan.0] and @racket[
 -nan.0], then it obeys the laws of an equivalence relation.}

@defproc[(equivalence-relation-map [relation equivalence-relation?]
                                   [f (-> any/c any/c)])
         equivalence-relation?]{
 Wraps @racket[relation] as an @tech{equivalence relation} that first applies
 @racket[f] to its inputs, then compares the returned results with @racket[
 relation].

 @(examples
   #:eval (make-evaluator) #:once
   (define string-length=
     (equivalence-relation-map natural-equality string-length))
   (equivalence-relation-holds? string-length= "hello" "world")
   (equivalence-relation-holds? string-length= "goodbye" "world"))}
