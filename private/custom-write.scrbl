#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/pretty
                     racket/struct
                     rebellion/custom-write
                     rebellion/custom-write/struct
                     rebellion/custom-write/tuple
                     rebellion/type/struct
                     rebellion/type/tuple)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/pretty
                   'rebellion/custom-write
                   'rebellion/custom-write/struct
                   'rebellion/custom-write/tuple
                   'rebellion/type/struct
                   'rebellion/type/tuple)
    #:private (list 'racket/base)))

@title{Custom Write Implementations}
@defmodule[rebellion/custom-write]

A @deftech{custom write implementation} is a function that prints values and is
suitable for use with @racket[prop:custom-write]. Custom write implementations
must satisfy the @racket[custom-write-function/c] contract.

@defthing[custom-write-function/c chaperone-contract?
          #:value (-> any/c output-port? custom-write-mode/c void?)]{
 A @tech/reference{contract} describing functions suitable for use with @racket[
 prop:custom-write].}

@defthing[custom-write-mode/c flat-contract?
          #:value (or/c boolean? 0 1)]{
 A @tech/reference{contract} describing the @racket[_mode] argument to functions matching
 @racket[custom-write-function/c]. See @racket[gen:custom-write] for details.}

@defproc[(make-named-object-custom-write
          [type-name symbol?]
          [#:name-getter get-name (-> any/c (or/c symbol? #f)) object-name])
         custom-write-function/c]{
 Constructs a @tech{custom write implementation} that prints values as opaque,
 unreadable, named objects, similar to the way functions are printed.

 @(examples
   #:eval (make-evaluator) #:once
   (struct person (name)
     #:property prop:object-name (struct-field-index name)
     #:property prop:custom-write (make-named-object-custom-write 'person))

   (person 'alyssa)
   (person 'jared)
   (person #f))}

@defproc[(make-constant-custom-write [name symbol?]) custom-write-function/c]{
 Constructs a @tech{custom write implementation} that prints all values as the
 same opaque constant value named @racket[name], similar to the way @racket[eof]
 prints.

 @(examples
   #:eval (make-evaluator) #:once
   (struct widget ()
     #:property prop:custom-write (make-constant-custom-write 'widget))

   (widget))}

@section{Struct Custom Write Implementations}
@defmodule[rebellion/custom-write/struct]

@defproc[(make-struct-constructor-style-custom-write
          [descriptor struct-descriptor?])
         custom-write-function/c]{
 Constructs a @tech{custom write implementation} that prints instances of the
 structure type described by @racket[descriptor] in a manner similar to the way
 that @racket[make-constructor-style-printer] prints values.

 @(examples
   #:eval (make-evaluator) #:once
   (define (make-props descriptor)
     (define custom-write
       (make-struct-constructor-style-custom-write descriptor))
     (list (cons prop:custom-write custom-write)))

   (define point-descriptor
     (make-struct-implementation #:name 'point
                                  #:immutable-fields 2
                                  #:property-maker make-props))
   (define point (struct-descriptor-constructor point-descriptor))

   (point 1 2)
   (parameterize ([pretty-print-columns 10])
     (pretty-print (point 100000000000000 200000000000000))))}

@section{Tuple Custom Write Implementations}
@defmodule[rebellion/custom-write/tuple]

@defproc[(make-tuple-named-object-custom-write
          [descriptor tuple-descriptor?]
          [#:name-field name-field (or/c natural? #f) #f])
         custom-write-function/c]{
 Constructs a @tech{custom write implementation} that prints instances of the
 @tech{tuple type} described by @racket[descriptor] in a manner similar to the
 way that @racket[make-named-object-custom-write] prints values. If @racket[
 name-field] is provided, it must refer to a field in the tuple type that will
 contain each instance's name. Otherwise, @racket[object-name] is used to
 extract the name of each instance.

 @(examples
   #:eval (make-evaluator) #:once
   (define-tuple-type person (name)
     #:property-maker
     (λ (descriptor)
       (list (cons prop:object-name 0)
             (cons prop:custom-write
                   (make-tuple-named-object-custom-write descriptor)))))

   (person 'alyssa)
   (person 'jared)
   (person #f))}
