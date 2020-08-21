#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/match
                     rebellion/base/symbol
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/wrapper)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'racket/match
                   'rebellion/type/wrapper)
    #:private (list 'racket/base)))

@title{Wrapper Types}
@defmodule[rebellion/type/wrapper]

A @deftech{wrapper type} is a kind of @tech{data type} for values that are
simple wrappers around other values. An instance of a wrapper type has a single
field containing the wrapped value, and two instances of the same wrapper type
are @racket[equal?] if they wrap @racket[equal?] values. Wrapper types are
useful when the same kind of data is used in many different ways that need to be
distinguished.

@(examples
  #:eval (make-evaluator) #:label #f
  (eval:no-prompt
   (define-wrapper-type celsius)
   (define-wrapper-type fahrenheit)
   code:blank
   (define/contract (celsius->fahrenheit c)
     (-> celsius? fahrenheit?)
     (fahrenheit (+ (* (celsius-value c) 9/5) 32))))

  (celsius->fahrenheit (celsius 0))
  (celsius->fahrenheit (celsius 100))
  (eval:error (celsius->fahrenheit (fahrenheit 100))))

@defform[
 (define-wrapper-type id option ...)

 #:grammar
 ([option
   #:omit-root-binding
   (code:line #:descriptor-name descriptor-id)
   (code:line #:predicate-name predicate-id)
   (code:line #:constructor-name constructor-id)
   (code:line #:accessor-name accessor-id)
   (code:line #:pattern-name pattern-id)
   (code:line #:property-maker prop-maker-expr)])

 #:contracts
 ([prop-maker-expr
   (-> uninitialized-wrapper-descriptor?
       (listof (cons/c struct-type-property? any/c)))])]{
 Creates a new @tech{wrapper type} named @racket[id] and binds the following
 identifiers:

 @itemlist[
 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate function that returns @racket[#t] when given instances of the
   created type and returns @racket[#f] otherwise.}
  
 @item{@racket[constructor-id], which defaults to @racketidfont{
    constructor:}@racket[id] --- a constructor function that wraps a value and
   returns an instance of the created type.}

 @item{@racket[accessor-id], which defaults to @racket[id]@racketidfont{-value}
   --- an accessor function that unwraps instances of the created type and
   returns their underlying value.}

 @item{@racket[pattern-id], which defaults to @racketidfont{pattern:}@racket[id]
   --- a @tech/reference{match expander} that unwraps instances of the created
   type and matches their contents against a subpattern.}]

 Additionally, unless @racket[#:omit-root-binding] is specified, the original
 @racket[id] is bound to @racket[pattern-id] when used in match patterns and to
 @racket[constructor-id] when used as an expression. Use @racket[
 #:omit-root-binding] when you want control over what @racket[id] is bound to,
 such as when creating a smart constructor.

 @(examples
   #:eval (make-evaluator) #:once
   (define-wrapper-type seconds)
   (seconds 10)
   (seconds-value (seconds 25))
   (seconds? (seconds 10))
   (match-define (seconds (? even? x)) (seconds 10))
   (eval:error (match-define (seconds (? even? x)) (seconds 25))))}

@section{Wrapper Type Information}

@defproc[(wrapper-type? [v any/c]) boolean?]{
 A predicate for @tech{wrapper types}.}

@defproc[(wrapper-type
          [name interned-symbol?]
          [#:predicate-name predicate-name (or/c interned-symbol? #f) #f]
          [#:constructor-name constructor-name (or/c interned-symbol? #f) #f]
          [#:accessor-name accessor-name (or/c interned-symbol? #f) #f])
         wrapper-type?]{
 Constructs a @tech{wrapper type} named @racket[name].
 The optional @racket[predicate-name], @racket[constructor-name], and
 @racket[accessor-name] arguments control the result of @racket[object-name] on
 the functions implementing the type. If not provided, @racket[predicate-name]
 defaults to @racket[name]@racketidfont{?}, @racket[constructor-name] defaults
 to @racket[name], and @racket[accessor-name] defaults to
 @racket[name]@racketidfont{-value}. Two wrapper types constructed with the same
 arguments are @racket[equal?]. To make an implementation of a wrapper type, see
 @racket[make-wrapper-implementation].}

@deftogether[[
 @defproc[(wrapper-type-name [type wrapper-type?]) interned-symbol?]
 @defproc[(wrapper-type-predicate-name [type wrapper-type?]) interned-symbol?]
 @defproc[(wrapper-type-constructor-name [type wrapper-type?]) interned-symbol?]
 @defproc[(wrapper-type-accessor-name [type wrapper-type?]) interned-symbol?]]]{
 Accessors for the various fields of a @tech{wrapper type}.}

@section{Wrapper Type Descriptors}

The @tech{type descriptor} for a @tech{wrapper type} contains two functions that
implement the type:

@itemlist[
 @item{A @deftech{wrapper constructor} that accepts one argument and constructs
  a wrapper instance.}

 @item{A @deftech{wrapper accessor} that accepts an instance of the wrapper type
  and returns the value inside the instance.}]

These functions can be used to dynamically construct and inspect instances of
arbitrary wrapper types at runtime, assuming the type's descriptor is
initialized.

@defproc[(wrapper-descriptor? [v any/c]) boolean?]{
 A predicate for @tech{type descriptors} of @tech{wrapper types}.}

@defproc[(initialized-wrapper-descriptor? [v any/c]) boolean?]{
 A predicate for initialized wrapper @tech{type descriptors}.}

@defproc[(uninitialized-wrapper-descriptor? [v any/c]) boolean?]{
 A predicate for uninitialized wrapper @tech{type descriptors}.}

@defproc[(make-wrapper-implementation
          [type wrapper-type?]
          [#:property-maker prop-maker
           (-> uninitialized-wrapper-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           default-wrapper-properties]
          [#:inspector inspector inspector? (current-inspector)])
         initialized-wrapper-descriptor?]{
 Implements @racket[type] and returns a @tech{type descriptor} for the new
 implementation. The @racket[inspector] argument behaves the same as in
 @racket[make-struct-type], although there are no transparent or prefab wrapper
 types. The @racket[prop-maker] argument is similar to the corresponding
 argument of @racket[make-struct-implementation]. By default, wrapper types are
 created with properties that make them print and compare in a manner similar to
 transparent structure types --- see @racket[default-wrapper-properties] for
 details.}

@defproc[(wrapper-descriptor-type [descriptor wrapper-descriptor?])
         wrapper-type?]{
 Returns the @tech{wrapper type} that @racket[descriptor] implements.}

@defproc[(wrapper-descriptor-predicate [descriptor wrapper-descriptor?])
         predicate/c]{
 Returns a predicate that returns true when given any wrapper instance created
 by @racket[descriptor]. The predicate is specific to @racket[descriptor] --- it
 will not return true for wrapper instances created by any other wrapper
 descriptors, even if they're different implementations of the same wrapper type
 as @racket[descriptor]. This is because wrapper types are
 @tech{nominal types}.}

@defproc[(wrapper-descriptor-constructor [descriptor wrapper-descriptor?])
         (-> any/c (wrapper-descriptor-predicate descriptor))]{
 Returns the @tech{wrapper constructor} of the wrapper type implementation
 represented by @racket[descriptor]. The constructor accepts one argument and
 returns an instance of the wrapper type.}

@defproc[(wrapper-descriptor-accessor [descriptor wrapper-descriptor?])
         (-> (wrapper-descriptor-predicate descriptor) any/c)]{
 Returns the @tech{wrapper accessor} of the wrapper type implementation
 represented by @racket[descriptor]. The accessor accepts wrapper instances
 created by @racket[descriptor] and returns the value inside the given
 instance.}

@defproc[(default-wrapper-properties [descriptor wrapper-descriptor?])
         (listof (cons/c struct-type-property? any/c))]{
 Returns implementations of @racket[prop:equal+hash] and @racket[
 prop:custom-write] suitable for most @tech{wrapper types}. This function is
 called by @racket[make-wrapper-implementation] when no @racket[_prop-maker]
 argument is supplied.}

@defproc[(default-wrapper-equal+hash [descriptor wrapper-descriptor?])
         equal+hash/c]{
 Builds an equality-checking function, a hashing function, and a secondary
 hashing function suitable for use with @racket[prop:equal+hash], each of which
 operate on instances of @racket[descriptor] by comparing and hashing their
 wrapped values. This causes @racket[equal?] to behave roughly the same as it
 does on transparent structure types. This function is used by
 @racket[default-wrapper-properties] to implement @racket[prop:equal+hash].}

@defproc[(default-wrapper-custom-write [descriptor wrapper-descriptor?])
         custom-write-function/c]{
 Constructs a @tech{custom write implementation} that prints instances of the
 @tech{wrapper type} described by @racket[descriptor] in a manner similar to
 transparent structures. This function is used by
 @racket[default-wrapper-properties] to implement @racket[prop:custom-write].}
