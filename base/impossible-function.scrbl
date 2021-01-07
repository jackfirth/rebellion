#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/impossible-function
                     rebellion/collection/list)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/impossible-function
                   'rebellion/collection/list)
    #:private (list 'racket/base)))

@title{Uncallable Functions}
@defmodule[rebellion/base/impossible-function]

@defproc[(impossible [v none/c]) any/c]{
 The impossible function. Calling @racket[impossible] with any input always
 raises an error, because it is impossible for @racket[v] to satisfy the
 @racket[none/c] @tech/reference{contract}.

 @(examples
   #:eval (make-evaluator)
   (eval:error (impossible 42)))

 This function is useful as an argument to higher-order functions where you know
 the function will not be called. For example, mapping the impossible function
 over an empty list will succeed without error:

 @(examples
   #:eval (make-evaluator)
   (map impossible empty-list))}
