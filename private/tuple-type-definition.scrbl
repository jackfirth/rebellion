#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/tuple-type
                     rebellion/tuple-type-definition)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/tuple-type-definition)
    #:private (list 'racket/base)))

@title{Tuple Type Definitions}
@defmodule[rebellion/tuple-type-definition]

@defform[
 (define-tuple-type id (field-id ...) option ...)
 #:grammar
 ([option (code:line #:constructor constructor-id)
   (code:line #:predicate predicate-id)
   (code:line #:property-maker prop-maker-expr)])
 #:contracts
 ([prop-maker-expr (-> uninitialized-tuple-descriptor?
                       (listof (cons/c struct-type-property? any/c)))])]{
 Defines a new @tech{tuple type}.

 @(examples
   #:eval (make-evaluator) #:once
   (define-tuple-type point (x y))
   (point 1 2)
   (point-x (point 42 0))
   (point-y (point 42 0)))}
