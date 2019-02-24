#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/list
                     racket/math
                     rebellion/struct-descriptor)
          rebellion/private/scribble-evaluator-factory
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/struct-descriptor)
    #:private (list 'racket/base)))

@title{Struct Descriptors}
@defmodule[rebellion/struct-descriptor]

@defproc[(struct-descriptor? [v any/c]) boolean?]{
 A predicate for structure type descriptors.}

@defproc[(struct-descriptor
          [#:type type struct-type?]
          [#:constructor constructor struct-constructor-procedure?]
          [#:predicate predicate struct-predicate-procedure?]
          [#:accessor accessor struct-accessor-procedure?]
          [#:mutator mutator struct-mutator-procedure?])
         struct-descriptor?]{
 Constructs a structure type descriptor for @racket[type].}

@deftogether[
 (@defproc[(struct-descriptor-type [descriptor struct-descriptor?])
           struct-type?]
   @defproc[(struct-descriptor-constructor [descriptor struct-descriptor?])
            struct-constructor-procedure?]
   @defproc[(struct-descriptor-predicate [descriptor struct-descriptor?])
            struct-predicate-procedure?]
   @defproc[(struct-descriptor-accessor [descriptor struct-descriptor?])
            struct-accessor-procedure?]
   @defproc[(struct-descriptor-mutator [descriptor struct-descriptor?])
            struct-mutator-procedure?])]{
 Accessors for the various fields of a structure type descriptor.}

@defproc[(make-struct-type/descriptor
          [#:name name symbol?]
          [#:mutable-fields mutable-fields natural? 0]
          [#:immutable-fields immutable-fields natural? 0]
          [#:auto-fields auto-fields natural? 0]
          [#:auto-field-value auto-value any/c #f]
          [#:super-type super-type (or/c struct-type? #f) #f]
          [#:properties props
           (listof (cons/c struct-type-property? any/c)) empty]
          [#:inspector inspector (or/c inspector? 'prefab #f) #f]
          [#:guard guard (or/c procedure? #f) #f]
          [#:constructor-name constructor-name (or/c symbol? #f) #f])
         struct-descriptor?]{
 Like @racket[make-struct-type], but with keyword arguments instead of
 positional arguments and returning a single @racket[struct-descriptor?] value
 instead of multiple values. Additional differences include:

 @itemlist[
 @item{Instead of fields defaulting to mutable and specifying a list of indices
   for immutable fields, this function accepts separate arguments for the number
   of mutable fields and the number of immutable fields. The created struct type
   puts all mutable fields before all immutable fields.}

 @item{The @racket[proc-spec] argument is not supported directly. This argument
   is made obsolete by @racket[prop:procedure]; instead of passing @racket[
 proc-spec] callers should include a value for @racket[prop:procedure] in
   @racket[props].}]

 @(examples
   #:eval (make-evaluator) #:once
   (define point-descriptor
     (make-struct-type/descriptor #:name 'point #:immutable-fields 2))
   (define point (struct-descriptor-constructor point-descriptor))
   (point 1 2))}
