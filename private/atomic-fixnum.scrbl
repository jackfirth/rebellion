#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/base/symbol
                     rebellion/concurrency/atomic/fixnum)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/decode
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/math
                   'rebellion/concurrency/atomic/fixnum)
    #:private (list 'racket/base)))

@(define (warn-about-possible-nontermination var-content)
   (decode-content
    @list{This operation is not guaranteed to terminate. The operation may be
 retried if @var-content is concurrently mutated, and there are no limits on how
 many times it may be retried.}))

@(define returns-status-boolean
   (decode-content
    @list{Returns a boolean indicating whether or not the operation
 succeeded.}))

@(define (warn-about-updater-retries updater-content)
   (decode-content
    @list{Because the update may be retried, @updater-content should be safe to
 apply multiple times. Beware that any side effects in @updater-content may be
 executed more than once.}))

@title{Atomic Fixnums}
@defmodule[rebellion/concurrency/atomic/fixnum]

An @deftech{atomic fixnum} is a thread-safe, future-safe, kill-safe, break-safe,
and lock-free mutable object containing a single @tech/reference{fixnum}. The
most basic operation on an atomic fixnum is a @deftech{compare and set}, which
attempts to set the atomic fixnum to a new value if and only if its current
value is equal to some specific value. When Racket is compiled with support for
@tech/reference{futures}, this operation is implemented with a single hardware
instruction.

All other operations on atomic fixnums are implemented in terms of atomic
compare and set with a retry loop. For example, @racket[atomic-fixnum-add!] gets
the current value of the atomic fixnum, adds it with an input number, then
attempts to set the value to the computed result if and only if the atomic
fixnum's current value at the time of the compare-and-set is equal to its value
at the time the result was computed. If that isn't the case due to contention
with competing threads or futures, then @racket[atomic-fixnum-add!] retries the
operation again. This is a form of Optimistic Concurrency Control (OCC).

@defproc[(atomic-fixnum? [v any/c]) boolean?]{
 A predicate for @tech{atomic fixnums}.}

@defproc[(make-atomic-fixnum [initial-value fixnum?]
                             [#:name name (or/c interned-symbol? #f) #f])
         atomic-fixnum?]{
 Constructs a new @tech{atomic fixnum} named @racket[name] and set to @racket[
 initial-value]. Providing a @racket[name] is recommended for debugging and
 logging purposes.}

@section{Basic Atomic Fixnum Operations}

@defproc[(atomic-fixnum-get [num atomic-fixnum?]) fixnum?]{
 Returns the current value of @racket[num].

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 42))
   (atomic-fixnum-get num))}

@defproc[(atomic-fixnum-set! [num atomic-fixnum?] [replacement fixnum?]) void?]{
 Changes the current value of @racket[num] to @racket[replacement].

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 0))
   (atomic-fixnum-set! num 5)
   (atomic-fixnum-get num))}

@defproc[(atomic-fixnum-add! [num atomic-fixnum?] [amount fixnum?]) void?]{
 Adds @racket[amount] to @racket[num].

 @warn-about-possible-nontermination{@racket[num]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 3))
   (atomic-fixnum-add! num 6)
   (atomic-fixnum-get num))}

@defproc[(atomic-fixnum-update! [num atomic-fixnum?]
                                [updater (-> fixnum? fixnum?)])
         void?]{
 Applies @racket[updater] to the current value of @racket[num] and sets @racket[
 num] to the value returned by @racket[updater].

 @warn-about-possible-nontermination{@racket[num]}

 @warn-about-updater-retries{@racket[updater]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 5))
   (atomic-fixnum-update! num sqr)
   (atomic-fixnum-get num))}

@section{Conditional Atomic Fixnum Operations}

@defproc[(atomic-fixnum-compare-and-set!
          [num atomic-fixnum?]
          [expected fixnum?]
          [replacement fixnum?])
         boolean?]{
 Attempts a @tech{compare and set} operation on @racket[num], setting
 it to @racket[replacement] if and only if its current value is equal to
 @racket[expected]. @returns-status-boolean

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 0))
   (atomic-fixnum-compare-and-set! num 1 42)
   (atomic-fixnum-get num)
   (atomic-fixnum-compare-and-set! num 0 42)
   (atomic-fixnum-get num))}

@defproc[(atomic-fixnum-compare-and-exchange!
          [num atomic-fixnum?]
          [expected fixnum?]
          [replacement fixnum?])
         fixnum?]{
 Attempts a compare and exchange operation on @racket[num], setting it to
 @racket[replacement] if and only if its current value is equal to @racket[
 expected]. Returns the value of @racket[num] before the exchange, which will be
 equal to @racket[replacement] if and only if the operation suceeeded.

 @warn-about-possible-nontermination{@racket[num]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 0))
   (atomic-fixnum-compare-and-exchange! num 1 42)
   (atomic-fixnum-get num)
   (atomic-fixnum-compare-and-exchange! num 0 42)
   (atomic-fixnum-get num))}

@defproc[(atomic-fixnum-compare-and-add!
          [num atomic-fixnum?]
          [expected fixnum?]
          [amount fixnum?])
         boolean?]{
 Attempts a compare and add operation on @racket[num], adding @racket[amount] to
 it if and only if its current value is equal to @racket[expected].
 @returns-status-boolean

 @warn-about-possible-nontermination{@racket[num]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 3))
   (atomic-fixnum-compare-and-add! num 2 6)
   (atomic-fixnum-get num)
   (atomic-fixnum-compare-and-add! num 3 6)
   (atomic-fixnum-get num))}

@section{Compound Atomic Fixnum Operations}

@defproc[(atomic-fixnum-get-then-set! [num atomic-fixnum?]
                                      [replacement fixnum?])
         fixnum?]{
 Sets @racket[num] to @racket[replacement] and returns its previous value.

 @warn-about-possible-nontermination{@racket[num]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 5))
   (atomic-fixnum-get-then-set! num 10)
   (atomic-fixnum-get num))}

@defproc[(atomic-fixnum-get-then-add! [num atomic-fixnum?] [amount fixnum?])
         fixnum?]{
 Adds @racket[amount] to @racket[num] and returns its previous value.

 @warn-about-possible-nontermination{@racket[num]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 3))
   (atomic-fixnum-get-then-add! num 6)
   (atomic-fixnum-get num))}

@defproc[(atomic-fixnum-add-then-get! [num atomic-fixnum?] [amount fixnum?])
         fixnum?]{
 Adds @racket[amount] to @racket[num] and returns its new value.

 @warn-about-possible-nontermination{@racket[num]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 3))
   (atomic-fixnum-add-then-get! num 6))}

@defproc[(atomic-fixnum-get-then-update! [num atomic-fixnum?]
                                         [updater (-> fixnum? fixnum?)])
         fixnum?]{
 Applies @racket[updater] to @racket[num] and returns its previous value.

 @warn-about-possible-nontermination{@racket[num]}
  
 @warn-about-updater-retries{@racket[updater]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 5))
   (atomic-fixnum-get-then-update! num sqr)
   (atomic-fixnum-get num))}

@defproc[(atomic-fixnum-update-then-get! [num atomic-fixnum?]
                                         [updater (-> fixnum? fixnum?)])
         fixnum?]{
 Applies @racket[updater] to @racket[num] and returns its new value.

 @warn-about-possible-nontermination{@racket[num]}
  
 @warn-about-updater-retries{@racket[updater]}

 @(examples
   #:eval (make-evaluator) #:once
   (define num (make-atomic-fixnum 5))
   (atomic-fixnum-update-then-get! num sqr))}
