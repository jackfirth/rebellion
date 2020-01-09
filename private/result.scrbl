#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/result)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/result)
    #:private (list 'racket/base)))

@title{Results}
@defmodule[rebellion/base/result]

A @deftech{result} is a wrapper around a value that represents either a
successful computation or a failed computation. Success results are constructed
by wrapping a value with @racket[success], and failure results are constructed
using @racket[failure]. The @racket[result-case] function provides a basic way
to unwrap a result. Results are useful when it is difficult to guarantee whether
a computation will succeed or fail and specifying a contract up front is either
expensive or impossible, such as in filesystem operations, text parsers, or
network requests. Wrapping returned values as a result instead of throwing
exceptions pushes callers to confront the failure case in order to unwrap the
value.

@defproc[(result? [v any/c]) boolean?]{
 A predicate for @tech{result} values.}

@defform[(result body ...+)]{
 Evaluates each @racket[body] form and returns the result of the last @racket[
 body], wrapped as a @racket[success] result. Definitions within the @racket[
 body] forms are locally scoped and aren't visible outside the @racket[result]
 expression. The last @racket[body] must be an expression that produces exactly
 one value.

 @(examples
   #:eval (make-evaluator) #:once
   (result
    (define foo 1)
    (define bar 2)
    (+ foo bar)))

 If any @racket[body] form raises an error, that error is wrapped as a @racket[
 failure] result and returned. Any remaining @racket[body] forms are not
 evaluated.

 @(examples
   #:eval (make-evaluator) #:once
   (result
    (define foo 1)
    (raise "oh no!")
    (define bar 2)
    (+ foo bar)))}

@defproc[(result-case [result result?]
                      [#:success success-handler (-> any/c any/c)]
                      [#:failure failure-handler (-> any/c any/c)])
         any/c]{
 Unwraps @racket[result], then applies either @racket[success-handler] or
 @racket[failure-handler] to the unwrapped value depending on whether
 @racket[result] is a successful result or a failed result.

 @(examples
   #:eval (make-evaluator) #:once
   (result-case (success 42) #:success add1 #:failure displayln)
   (result-case (failure "oh no!") #:success add1 #:failure displayln))}

@defproc[(result/c [success-contract chaperone-contract?]
                   [failure-contract chaperone-contract?]) chaperone-contract?]{
 Constructs a contract that accepts successful @tech{result} values matching
 @racket[success-contract] and failed result values matching @racket[
 failure-contract]. Equivalent to @racket[
 (or/c (success/c success-contract) (failure/c failure-contract))].}

@section{Successful Results}

@defproc[(success? [v any/c]) boolean?]{
 A predicate for successful @tech{result} values. Implies @racket[result?].}

@defproc[(success [v any/c]) success?]{
 Constructs a successful @tech{result}.}

@defproc[(success-value [succ success?]) any/c]{
 Returns the value wrapped by @racket[succ].}

@defproc[(success/c [contract chaperone-contract?]) chaperone-contract?]{
 Constructs a contract that accepts successful @tech{result} values matching
 @racket[contract].}

@section{Failed Results}

@defproc[(failure? [v any/c]) boolean?]{
 A predicate for failed @tech{result} values. Implies @racket[result?].}

@defproc[(failure [err any/c]) failure?]{
 Constructs a failed @tech{result}.}

@defproc[(failure-error [fail failure?]) any/c]{
 Returns the error value wrapped by @racket[fail].}

@defproc[(failure/c [contract chaperone-contract?]) chaperone-contract?]{
 Constructs a contract that accepts failed @tech{result} values matching
 @racket[contract].}
