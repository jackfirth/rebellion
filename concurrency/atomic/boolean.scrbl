#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/concurrency/atomic/boolean)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/concurrency/atomic/boolean)
    #:private (list 'racket/base)))

@title{Atomic Booleans}
@defmodule[rebellion/concurrency/atomic/boolean]

An @deftech{atomic boolean} is a thread-safe, future-safe, kill-safe,
break-safe, and wait-free mutable object containing a single @tech/reference{
 boolean}.

@defproc[(atomic-boolean? [v any/c]) boolean?]{
 A predicate for @tech{atomic booleans}.}

@defproc[(make-atomic-boolean [initial-value boolean?]) atomic-boolean?]{
 Constructs a new @tech{atomic boolean} and sets it to @racket[initial-value].}

@defproc[(atomic-boolean-get [bool atomic-boolean?]) boolean?]{
 Returns the current value of @racket[bool].

 @(examples
   #:eval (make-evaluator) #:once
   (define bool (make-atomic-boolean #false))
   (atomic-boolean-get bool))}

@defproc[(atomic-boolean-set! [bool atomic-boolean?] [replacement boolean?])
         void?]{
 Sets the current value of @racket[bool] to @racket[replacement].

 @(examples
   #:eval (make-evaluator) #:once
   (define bool (make-atomic-boolean #false))
   (atomic-boolean-set! bool #true)
   (atomic-boolean-get bool))}

@defproc[(atomic-boolean-compare-and-set! [bool atomic-boolean?]
                                          [expected boolean?]
                                          [replacement boolean?])
         boolean?]{
 Attempts to set @racket[bool] to @racket[replacement], succeeding if and only
 if its current value is @racket[expected]. Returns a boolean indicating whether
 or not the operation succeeded.

 @(examples
   #:eval (make-evaluator) #:once
   (define bool (make-atomic-boolean #true))
   (atomic-boolean-compare-and-set! bool #false #true)
   (atomic-boolean-get bool)
   (atomic-boolean-compare-and-set! bool #true #false)
   (atomic-boolean-get bool))}

@defproc[(atomic-boolean-compare-and-exchange! [bool atomic-boolean?]
                                               [expected boolean?]
                                               [replacement boolean?])
         boolean?]{
 Attempts to set @racket[bool] to @racket[replacement], succeeding if and only
 if its current value is @racket[expected]. Returns the value of @racket[bool]
 before the exchange. If the returned value is equal to @racket[expected], that
 indicates the exchange succeeded.

 @(examples
   #:eval (make-evaluator) #:once
   (define bool (make-atomic-boolean #false))
   (atomic-boolean-compare-and-exchange! bool #true #false)
   (atomic-boolean-get bool)
   (atomic-boolean-compare-and-exchange! bool #false #true)
   (atomic-boolean-get bool))}

@defproc[(atomic-boolean-get-then-set! [bool atomic-boolean?]
                                       [replacement boolean?])
         boolean?]{
 Sets @racket[bool] to @racket[replacement] and returns its previous value.

 @(examples
   #:eval (make-evaluator) #:once
   (define bool (make-atomic-boolean #false))
   (atomic-boolean-get-then-set! bool #true)
   (atomic-boolean-get bool))}
