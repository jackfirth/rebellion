#lang scribble/manual

@(require (for-label racket/base
                     rebellion/singleton)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/singleton)
    #:private (list 'racket/base)))

@title{Singletons}
@defmodule[rebellion/singleton]

@defform[
 (define-singleton-type id singleton-option ...)
 #:grammar ([singleton-option
             (code:line #:name name)
             (code:line #:predicate-name predicate-name)
             (code:line #:descriptor-name descriptor-name)
             (code:line #:type-representation-name type-representation-name)
             (code:line #:inspector inspector)
             (code:line #:property-maker property-maker)])
 #:contracts ([inspector inspector?]
              [property-maker
               (-> uninitialized-singleton-descriptor?
                   (listof (cons/c struct-type-property? any/c)))])]{
 Defines a @tech{singleton type} and binds the following identifiers:

 @itemlist[

 @item{@racket[name] (which defaults to @racket[id]), the singleton instance.}

 @item{@racket[predicate-name] (which defaults to @racket[id]@racketidfont{?}),
   a predicate that returns @racket[#t] for the singleton instance and false for
   all other values.}

 @item{@racket[descriptor-name] (which defaults to
   @racketidfont{descriptor:}@racket[id]), a @tech{singleton type descriptor}
   for the defined singleton type.}

 @item{@racket[type-representation-name] (which defaults to
   @racketidfont{type:}@racket[id]), the defined @tech{singleton type}.}]

 @(examples
   #:eval (make-evaluator) #:once
   (define-singleton-type infinity)
   
   infinity
   (infinity? infinity)
   descriptor:infinity
   type:infinity)}
