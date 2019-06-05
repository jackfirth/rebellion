#lang scribble/manual

@(require (for-label racket/base
                     racket/math
                     rebellion/type/tuple
                     rebellion/type/struct)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/type/tuple)
    #:private (list 'racket/base)))

@title{Tuple Types}
@defmodule[rebellion/type/tuple]

A @deftech{tuple type} is a data type representing immutable fixed-size
sequences of unnamed values, each of which is a @deftech{tuple instance} of the
tuple type. All instances of a tuple type are the same size, and that size is
accessible from the type using @racket[tuple-type-size].

Tuple types are specifications, not implementations. A tuple type does not
directly provide a constructor or accessor; it only provides information about
the type such as its symbolic name, the number of fields, etc. An actual
implementation of a tuple type is obtained via @racket[
 make-tuple-implementation], which generates a new implementation of a
tuple type and returns a @deftech{tuple type descriptor} object. Descriptors
provide functions that implement the type and a predicate that identifies the
implementation's values. Descriptors are generative; calling @racket[
 make-tuple-implementation] with the same tuple type will make multiple
distinct implementations of that type.

@defproc[(tuple-type? [v any/c]) boolean?]{
 A predicate for @tech{tuple types}.}

@defproc[
 (tuple-type [name symbol?]
             [size natural?]
             [#:predicate-name predicate-name (or/c symbol? #f) #f]
             [#:constructor-name constructor-name (or/c symbol? #f) #f]
             [#:accessor-name accessor-name (or/c symbol? #f) #f])
 tuple-type?]{
 Constructs a @tech{tuple type} of size @racket[size] and named @racket[name].
 The optional @racket[predicate-name], @racket[constructor-name], and @racket[
 accessor-name] arguments control the result of @racket[object-name] on the
 functions implementing the type. If not provided, @racket[predicate-name]
 defaults to @racket[name]@verbatim|{?}|, @racket[constructor-name] defaults to
 @racket[name], and @racket[accessor-name] defaults to @racket[
 name]@verbatim|{-ref}|. Two tuple types constructed with the same arguments are
 @racket[equal?]. To make an implementation of a tuple type, see @racket[
 make-tuple-implementation].}

@deftogether[[
 @defproc[(tuple-type-name [type tuple-type?]) symbol?]
 @defproc[(tuple-type-size [type tuple-type?]) natural?]
 @defproc[(tuple-type-predicate-name [type tuple-type?]) symbol?]
 @defproc[(tuple-type-constructor-name [type tuple-type?]) symbol?]
 @defproc[(tuple-type-accessor-name [type tuple-type?]) symbol?]]]{
 Accessors for the various fields of a @tech{tuple type}.}

@defproc[(tuple-descriptor? [v any/c]) boolean?]{
 A predicate for @tech{tuple type descriptors}.}

@deftogether[[
 @defproc[(initialized-tuple-descriptor? [v any/c]) boolean?]
 @defproc[(uninitialized-tuple-descriptor? [v any/c]) boolean?]]]{
 Predicates for @tech{tuple type descriptors} that either have or haven't been
 initialized yet. The predicate, constructor, and accessor of an uninitialized
 tuple descriptor @bold{must not be called until initialization is complete}, as
 the implementations of those functions won't exist until then. Initialization
 of a descriptor completes when @racket[make-tuple-implementation]
 returns.}

@deftogether[[
 @defproc[(tuple-descriptor-type [descriptor tuple-descriptor?]) tuple-type?]
 @defproc[(tuple-descriptor-predicate [descriptor tuple-descriptor?])
          (-> any/c boolean?)]
 @defproc[(tuple-descriptor-constructor [descriptor tuple-descriptor?])
          procedure?]
 @defproc[(tuple-descriptor-accessor [descriptor tuple-descriptor?])
          (-> (tuple-descriptor-predicate descriptor) natural? any/c)]]]{
 Accessors for the various fields of a @tech{tuple type descriptor}.}

@defproc[
 (make-tuple-implementation
  [type tuple-type?]
  [#:guard guard (or/c procedure? #f) #f]
  [#:inspector inspector inspector? (current-inspector)]
  [#:property-maker prop-maker
   (-> uninitialized-tuple-descriptor?
       (listof (cons/c struct-type-property? any/c)))
   make-default-tuple-properties])
 initialized-tuple-descriptor?]{
 Implements @racket[type] and returns a @tech{tuple type descriptor} for the
 new implementation. The @racket[guard] and @racket[inspector] arguments behave
 the same as the corresponding arguments of @racket[make-struct-type], although
 there are no transparent or prefab tuple types. The @racket[prop-maker]
 argument is similar to the corresponding argument of @racket[
 make-struct-type/descriptor]. By default, tuple types are created with
 properties that make them print and compare in a manner similar to transparent
 structure types --- see @racket[make-default-tuple-properties] for details.

 @(examples
   #:eval (make-evaluator) #:once
   (define point-descriptor
     (make-tuple-implementation (tuple-type 'point 2)))
   (define point (tuple-descriptor-constructor point-descriptor))
   (define point-x (make-tuple-field-accessor point-descriptor 0 'x))
   (define point-y (make-tuple-field-accessor point-descriptor 1 'y))
   (point 42 888)
   (point-x (point 42 888))
   (point-y (point 42 888)))}

@defproc[
 (make-tuple-field-accessor [descriptor tuple-descriptor?]
                            [pos natural?]
                            [name (or/c symbol? #f)
                             (symbol->string (format "field~a" pos))])
 (-> (tuple-descriptor-predicate descriptor) any/c)]{
 Builds a field accessor function that returns the @racket[pos] field of
 instances of the @tech{tuple type} implemented by @racket[descriptor]. If
 @racket[name] is provided, it is used to derive the name of the function for
 debugging purposes. See @racket[make-tuple-implementation] for usage
 examples.}

@defproc[
 (make-default-tuple-properties [descriptor uninitialized-tuple-descriptor?])
 (listof (cons/c struct-type-property? any/c))]{
 Returns implementations of @racket[prop:equal+hash] and @racket[
 prop:custom-write] suitable for most @tech{tuple types}. This function is
 called by @racket[make-tuple-implementation] when no @racket[_prop-maker]
 argument is supplied.}

@defform[
 (define-tuple-type id (field-id ...) option ...)
 #:grammar
 ([option (code:line #:constructor-name constructor-id)
   (code:line #:predicate-name predicate-id)
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
