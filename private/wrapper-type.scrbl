#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/type/wrapper)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/type/wrapper)
    #:private (list 'racket/base)))

@title{Wrapper Types}
@defmodule[rebellion/type/wrapper]

@defform[
 (define-wrapper-type id option ...)
 #:grammar ([option (code:line #:constructor-name constructor-id)
             (code:line #:accessor-name accessor-id)
             (code:line #:predicate-name predicate-id)
             (code:line #:property-maker prop-maker-expr)])
 #:contracts ([prop-maker-expr
               (-> uninitialized-wrapper-descriptor?
                   (listof (cons/c struct-type-property? any/c)))])]{

 @(examples
   #:eval (make-evaluator) #:once
   (define-wrapper-type seconds)
   (seconds 10)
   (seconds-value (seconds 25))
   (seconds? (seconds 10)))}
