#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/pretty
                     racket/struct
                     rebellion/custom-write
                     rebellion/type/struct
                     rebellion/type/tuple)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/pretty
                   'rebellion/custom-write
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
 A @tech/reference{contract} describing the @racket[_mode] argument to functions
 matching @racket[custom-write-function/c]. See @racket[gen:custom-write] for
 details.}

@defthing[object-name/c flat-contract?
          #:value (or/c symbol? string? bytes? number? path? #false)]{
 A @tech/reference{contract} describing expected return values of
 @racket[object-name]. Note that @racket[object-name] can technically return any
 kind of value, so more precisely this contract describes the name values that
 @racket[make-named-object-custom-write] will accept.}

@defproc[(make-named-object-custom-write
          [type-name symbol?]
          [#:name-getter get-name (-> any/c object-name/c) object-name])
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
   (person #false))}
