#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/type/property-module)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/type/property-module)
    #:private (list 'racket/base)))

@title{Property Modules}
@defmodule[rebellion/type/property-module]

@defproc[(property-module? [v any/c]) boolean?]

@defproc[(property-module [binding property-binding?] ...) property-module?]

@defproc[(property-module-outputs [mod property-module?])
         (immutable-set/c struct-type-property?)]

@defthing[empty-property-module property-module?]

@defproc[(property-module-add-binding [mod property-module?]
                                      [binding property-binding?])
         property-module?]

@defproc[(property-module-instantiate [mod property-module?])
         (hash/c struct-type-property? any/c)]

@section{Property Bindings}

@defproc[(property-binding? [v any/c]) boolean?]

@defproc[(property-binding [property struct-type-property?]
                           [maker procedure?]
                           [dependency struct-type-property?] ...)
         property-binding?]

@defproc[(constant-property-binding [property struct-type-property?] [v any/c])
         property-binding?]

@defproc[(property-binding-output [binding property-binding?])
         struct-type-property?]
