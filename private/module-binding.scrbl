#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/set
                     rebellion/module/binding
                     rebellion/module/phase)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/module/binding)
    #:private (list 'racket/base)))

@title{Module Bindings}
@defmodule[rebellion/module/binding]

@defproc[(module-binding? [v any/c]) boolean?]

@defproc[(module-binding [path module-path?]
                         [phase execution-phase?]
                         [name symbol?])
         module-binding?]

@defproc[(module-binding-path [binding module-binding?]) module-path?]
@defproc[(module-binding-phase [binding module-binding?]) phase?]
@defproc[(module-binding-name [binding module-binding?]) symbol?]

@defproc[(module-bindings [mod module-path?]) (set/c module-binding?)]{
 Returns the set of bindings currently defined by @racket[mod], including both
 provided bindings and internal bindings.

 @(examples
   #:eval (make-evaluator) #:once
   (module-bindings 'racket/vector))}

@defproc[(module-provided-bindings [mod module-path?])
         (set/c module-binding?)]{
 Returns the set of bindings currently defined and exported by @racket[mod] with
 @racket[provide].

 @(examples
   #:eval (make-evaluator) #:once
   (module-provided-bindings 'racket/vector))}

@defproc[(module-internal-bindings [mod module-path?])
         (set/c module-binding?)]{
 Returns the set of bindings currently defined by @racket[mod] but @emph{not}
 provided.

 @(examples
   #:eval (make-evaluator) #:once
   (module-internal-bindings 'racket/vector))}
