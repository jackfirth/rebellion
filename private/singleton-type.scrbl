#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/math
                     rebellion/base/symbol
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/singleton)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'rebellion/type/singleton)
    #:private (list 'racket/base)))

@title{Singleton Types}
@defmodule[rebellion/type/singleton]

A @deftech{singleton type} is a simple kind of @tech{data type} made up of only
one value. Singleton types are useful for representing named constants, such as
the end-of-file marker value @racket[eof]. Symbols can also be used for this
purpose, but a singleton type is often preferable because misspelling the name
of the constant causes a compile error rather than a silent bug at runtime.

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define-singleton-type undefined)

   (define/contract (divide x y)
     (-> real? real? (or/c real? undefined?))
     (if (zero? y)
         undefined
         (/ x y))))

  (divide 6 3)
  (divide 6 0))

@defform[
 (define-singleton-type id singleton-option ...)
 #:grammar ([singleton-option
             (code:line #:predicate-name predicate-id)
             (code:line #:inspector inspector)
             (code:line #:property-maker property-maker)])
 #:contracts ([inspector inspector?]
              [property-maker
               (-> uninitialized-singleton-descriptor?
                   (listof (cons/c struct-type-property? any/c)))])]{
 Creates a @tech{singleton type} and binds the following identifiers:

 @itemlist[
 @item{@racket[id] --- the unique instance of the created type.}

 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate that returns @racket[#t] when given the singleton instance and
   false for all other values.}]

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define-singleton-type infinity))

   infinity
   (infinity? infinity))}

@section{Singleton Type Information}

@defproc[(singleton-type? [v any/c]) boolean?]{
 A predicate for @tech{singleton types}.}

@defproc[(singleton-type
          [name interned-symbol?]
          [#:predicate-name predicate-name (or/c interned-symbol? #f) #f])
         singleton-type?]{
 Constructs a @tech{singleton type} named @racket[name]. The optional
 @racket[predicate-name] argument controls the result of @racket[object-name] on
 a predicate implementing the type. It defaults to
 @racket[name]@racketidfont{?}. This function only constructs the information
 representing a singleton type; to implement the type, use
 @racket[make-singleton-implementation].}

@defproc[(singleton-type-name [type singleton-type?]) interned-symbol?]{
 Returns the name of @racket[type].}

@defproc[(singleton-type-predicate-name [type singleton-type?])
         interned-symbol?]{
 Returns the name that should be used for predicates implementing
 @racket[type].}

@section{Singleton Type Descriptors}

Singleton types are implemented using fieldless structs, where only a single
instance of the struct can be created. The @tech{type descriptor} for a
@tech{singleton type} provides a predicate and, if the descriptor is
initialized, can be used to retrieve the singleton instance.

@defproc[(singleton-descriptor? [v any/c]) boolean?]{
 A predicate for singleton @tech{type descriptors}.}

@defproc[(initialized-singleton-descriptor? [v any/c]) boolean?]{
 A predicate for initialized singleton @tech{type descriptors}.}

@defproc[(uninitialized-singleton-descriptor? [v any/c]) boolean?]{
 A predicate for uninitialized singleton @tech{type descriptors}.}

@defproc[(make-singleton-implementation
          [type singleton-type?]
          [#:inspector inspector inspector? (current-inspector)]
          [#:property-maker prop-maker
           (-> uninitialized-singleton-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           default-singleton-properties])
         initialized-singleton-descriptor?]{
 Implements @racket[type] and returns a @tech{type descriptor} for the new
 implementation. The @racket[inspector] argument behaves the same as in
 @racket[make-struct-type], although there are no transparent or prefab
 singleton types. The @racket[prop-maker] argument is similar to the
 corresponding argument of @racket[make-struct-implementation]. By default,
 singleton types are created with properties that make them print like other
 named Racket constants --- see @racket[default-singleton-properties] for
 details.}

@defproc[(singleton-descriptor-type [descriptor singleton-descriptor?])
         singleton-type?]{
 Returns the @tech{singleton type} that @racket[descriptor] implements.}

@defproc[(singleton-descriptor-predicate [descriptor singleton-descriptor?])
         predicate/c]{
 Returns a predicate that returns true when given the singleton instance created
 by @racket[descriptor]. The predicate is specific to @racket[descriptor] --- it
 will not return true for singleton instances created by any other singleton
 descriptors, even if they're different implementations of the same singleton
 type as @racket[descriptor]. This is because singleton types are
 @tech{nominal types}.}

@defproc[(singleton-descriptor-instance
          [descriptor initialized-singleton-descriptor?])
         (singleton-descriptor-predicate descriptor)]{
 Returns the instance of the singleton type implemented by @racket[descriptor].}

@defproc[(default-singleton-properties [descriptor singleton-descriptor?])
         (listof (cons/c struct-type-property? any/c))]{
 Returns implementations of @racket[prop:equal+hash],
 @racket[prop:custom-write], and @racket[prop:object-name] suitable for most
 @tech{singleton types}. The default implementation of @racket[prop:equal+hash]
 always returns true, since it's only called if two values are instances of the
 same type and singleton types only have one value.

 This function is called by @racket[make-singleton-implementation] when no
 @racket[_prop-maker] argument is supplied.}

@defproc[(default-singleton-custom-write [descriptor singleton-descriptor?])
         custom-write-function/c]{
 Builds a @tech{custom write implementation} that prints singleton instances
 created by @racket[descriptor] in a manner similar to other named Racket
 constants such as @racket[eof]. This function is used by
 @racket[default-singleton-properties] to implement @racket[prop:custom-write].}

@defproc[(default-singleton-object-name [descriptor singleton-descriptor?])
         (or/c natural? (-> any/c any/c))]{
 Builds a value suitable for use with @racket[prop:object-name] that causes
 @racket[object-name] to return the name of the instance when used on the
 singleton instance created by @racket[descriptor]. This function is used by
 @racket[default-singleton-properties] to implement @racket[prop:object-name].}
