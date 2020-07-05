#lang scribble/manual

@(require (for-label racket/base
                     rebellion/type/record
                     rebellion/type/singleton
                     rebellion/type/tuple
                     rebellion/type/wrapper)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/type/record
                   'rebellion/type/singleton
                   'rebellion/type/tuple
                   'rebellion/type/wrapper)
    #:private (list 'racket/base)))

@title[#:style (list 'toc)]{Data Types}
@defmodule[rebellion/type]

A @deftech{data type} or simply @deftech{type} is a set of values together with
1) some basic operations on those values and 2) a predicate to test whether a
value is within that set. There are many different kinds of data types, with
different basic operations and meant for different use cases.

@(examples
  #:eval (make-evaluator) #:once #:label #f
  (define-tuple-type point (x y))
  (point 7 42))

@(examples
  #:eval (make-evaluator) #:once #:label #f
  (define-record-type circle (radius color))
  (circle #:radius 5 #:color 'red))

@(examples
  #:eval (make-evaluator) #:once #:label #f
  (define-wrapper-type inches)
  (inches 17))

@(examples
  #:eval (make-evaluator) #:once #:label #f
  (define-singleton-type undefined)
  undefined)

@local-table-of-contents[]

@section{Interfaces Common to All Types}

The @racketmodname[rebellion/type] module is broken down into a collection of
modules, such as @racketmodname[rebellion/type/record] and @racketmodname[
 rebellion/type/tuple]. Each module is meant for working with a specific kind of
data type. However, all of these modules have a few things in common.

@subsection{Nominal v.s. Structural Types}

Any type created in @racketmodname[rebellion/type] is a @deftech{nominal type},
not a @deftech{structural type}. This means creating a new type, such as with
@racket[define-record-type], creates a new named unique type that is distinct
from all other types. The functions created by one use of @racket[
 define-record-type] will not work on instances created via another use of
@racket[define-record-type], even if both types are named the same and have
exactly the same fields.

@subsection{Struct-Based Types and Struct Type Properties}

All types are created using Racket @tech/reference{structure types}, and the
created struct types can have @tech/reference{structure type properties}
attached to them. Each module typically provides default structure type
properties for the types it creates, based on how its types are typically used.
These defaults can be freely overriden when desired.

@subsection{Type Implementations and Generativity}

The structure, or @deftech{shape} of a type is distinct from an @deftech{
 implementation} of that type. Each @racketmodname[rebellion/type] module
reflects this distinction by providing two different interfaces for shapes and
implementations. A @racketmodname[rebellion/type] module for working with @var[
 kind] types provides:

@itemlist[
 @item{A @racket[_kind]@racketidfont{-type?} type which describes the shape of a
  @racket[_kind] type but does not provide any constructors, accessors, or
  predicates. Two @racket[_kind]@racketidfont{-type?} values are equal whenever
  the names, field counts, etc. of the types they describe are equivalent. That
  is, @racket[_kind]@racketidfont{-type?} values are compared using structural
  equality. The term @deftech{shape of a type} or simply @tech{shape}, refers
  to a @racket[_kind]@racketidfont{-type?} value. When used without any
  qualification, the term @tech{type} typically refers to such a value as
  well.}

 @item{A @racket[_kind]@racketidfont{-descriptor?} type which provides an actual
  implementation of a @racket[_kind] type, including a predicate and any
  relevant constructors and accessors. Such an implementation is called a
  @deftech{type descriptor}, and allows generically operating on unknown types
  at runtime. At runtime, the term @deftech{implementation of a type}, or
  simply @tech{implementation}, refers to a type descriptor.}]

A @racket[_kind] descriptor can be created for a type using the
@racketidfont{make-}@racket[_kind]@racketidfont{-implementation} function
provided by the corresponding @racketmodfont{rebellion/type/}@racket[_kind]
module. Multiple calls to such a function with the same type will produce
distinct implementations that are not @racket[equal?] to each other, meaning
that the @racketmodname[rebellion/type] modules create @deftech{generative
 types}.

Type descriptors may be @emph{uninitialized}. An uninitialized type descriptor
cannot be used to create or interact with instances of the type until after
initialization is complete. Uninitialized type descriptors are fairly rare: they
can only be obtained via the @racket[#:property-maker] mechanism for specifying
structure type properties of created types. This is because the property-making
function needs to receive the type descriptor in order to return implementations
of type properties, but this happens @emph{before} the type is created. Property
makers can't use the descriptor's constructor and accessor immediately, but they
can refer to them in implementations of properties such as
@racket[prop:custom-write] (since by the time the constructor or accessor is
actually used, the type is created and the descriptor is initialized). This
delayed initialization step is necessary to allow property makers to be defined
without mutual recursion and in separate modules from the type definitions
they're used for, allowing reuse of generic property makers.

@subsection{Defining Types}

Each @racketmodfont{rebellion/type/}@racket[_kind] module provides a
@racketidfont{define-}@racket[_kind]@racketidfont{-type} form that creates a new
type and binds its constructor, predicate, and accessors to variables. These
forms are how most users of the @racketmodname[rebellion/type] modules are
expected to create types, similar to how most Racket struct types are created
with the @racket[struct] form rather than the dynamic @racket[make-struct-type]
function.

@subsection{Compile-Time Type Information}

At present, no means of representing compile-time information about types is
provided. As a result various related features, such as integration with
@racket[match] are not provided by any @racketmodname[rebellion/type] modules.
However, both a basic represention of compile-time type information and
integration with @racket[match] are intended to happen eventually. More advanced
functionality such as a full static type system and compile-time type checker
are out of scope for now, but it is hoped that such an effort can either build
on or integrate with this library.

@include-section[(lib "rebellion/private/record-type.scrbl")]
@include-section[(lib "rebellion/private/tuple-type.scrbl")]
@include-section[(lib "rebellion/private/enum-type.scrbl")]
@include-section[(lib "rebellion/private/singleton-type.scrbl")]
@include-section[(lib "rebellion/private/wrapper-type.scrbl")]
@include-section[(lib "rebellion/private/object-type.scrbl")]
@include-section[(lib "rebellion/private/struct-type.scrbl")]
