#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/option)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/option)
    #:private (list 'racket/base)))

@title{Option Values}
@defmodule[rebellion/base/option]

An @deftech{option} is an optional value that is either @deftech{present} or
@deftech{absent}. Present values are constructed with the @racket[present]
function, and absent values are represented by the @racket[absent] constant.

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define (hash-ref-option h k)
    (if (hash-has-key? h k)
        (present (hash-ref h k))
        absent)))

  (hash-ref-option (hash 'a 1 'b 2) 'a)
  (hash-ref-option (hash 'a 1 'b 2) 'c))

@defproc[(option? [v any/c]) boolean?]{
 A predicate for @tech{option} values, which are either @tech{present} or
 @tech{absent}.}

@defproc[(present? [v any/c]) boolean?]{
 A predicate for @tech{present} values. Implies @racket[option?].}

@defproc[(present [v any/c]) present?]{
 Constructs a @tech{present} value containing @racket[v].}

@defproc[(present-value [pres present?]) any/c]{
 Returns the value wrapped by @racket[pres].}

@defproc[(absent? [v any/c]) boolean?]{
 A predicate for the @tech{absent} value. Implies @racket[option?].}

@defthing[absent absent?]{
 The @tech{absent} constant.}

@defproc[(option-case [opt option?]
                      [#:present present-handler (-> any/c any/c)]
                      [#:absent absent-handler (-> any/c)])
         any/c]{
 Inspects @racket[opt] and, if it is @tech{present}, calls @racket[
 present-handler] on the present value. If it is @tech{absent}, calls @racket[
 absent-handler]. Returns the result of the called handler.

 @(examples
   #:eval (make-evaluator) #:once
   (option-case (present 42)
                #:present add1
                #:absent (λ () (error "missing!")))
   (eval:error
    (option-case absent
                 #:present add1
                 #:absent (λ () (error "missing!")))))}

@defproc[(option-map [opt option?] [f (-> any/c any/c)]) option?]{
 Applies @racket[f] to the value contained in @racket[opt] if it is @tech{
  present} and returns the result wrapped in a @tech{present} value. If @racket[
 opt] is @tech{absent}, returns @racket[absent].

 @(examples
   #:eval (make-evaluator) #:once
   (option-map (present 42) add1)
   (option-map absent add1))}

@defproc[(option-flat-map [opt option?] [f (-> any/c option?)]) option?]{
 Applies @racket[f] to the value contained in @racket[opt] if it is @tech{
  present} and returns the result of @racket[f]. If @racket[opt] is @tech{
  absent}, returns @racket[absent].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define (inverse x)
      (if (zero? x)
          absent
          (present (/ 1 x)))))
   (option-flat-map (present 42) inverse)
   (option-flat-map absent inverse)
   (option-flat-map (present 0) inverse))}

@defproc[(option-filter [opt option?] [pred predicate/c]) option?]{
 If @racket[opt] is @tech{present}, tests its value with @racket[pred] and
 returns it if @racket[pred] results in @racket[#t]. Otherwise, returns @racket[
 absent].

 @(examples
   #:eval (make-evaluator) #:once
   (option-filter (present 42) number?)
   (option-filter (present "hello") number?)
   (option-filter absent number?))}

@defproc[(option-get [opt option?] [default any/c]) any/c]{
 Returns the value in @racket[opt] if it is @tech{present}, otherwise returns
 @racket[default].

 @(examples
   #:eval (make-evaluator) #:once
   (option-get (present 42) 0)
   (option-get absent 0))}

@section{Contracts for Option Values}

@defproc[(option/c [contract chaperone-contract?]) chaperone-contract?]{
 Constructs a contract for @tech{option} values that accepts either the @tech{
  absent} value or a @tech{present} value that satisfies @racket[contract].
 Equivalent to @racket[(or/c absent? (present/c contract))].}

@defproc[(present/c [contract chaperone-contract?]) chaperone-contract?]{
 Constructs a contract for @tech{present} values that satisfy @racket[
 contract].}
