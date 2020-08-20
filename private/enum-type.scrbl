#lang scribble/manual

@(require (for-syntax racket/base
                      racket/symbol)
          (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/match
                     rebellion/base/symbol
                     rebellion/collection/keyset
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/enum
                     rebellion/type/enum/binding
                     rebellion/type/tuple
                     syntax/parse
                     syntax/parse/define)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-index-attribute doc)
          scribble/example
          syntax/parse/define)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'racket/match
                   'rebellion/collection/keyset
                   'rebellion/type/enum
                   'rebellion/type/tuple
                   'syntax/parse/define)
    #:private (list 'racket/base)))

@title{Enum Types}
@defmodule[rebellion/type/enum]

An @deftech{enum type} is a simple kind of @tech{data type} made up of a small
fixed set of named values. Enum types are useful for representing groups of
related constants, such as primary colors and directions.

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define-enum-type direction (up down left right))
   (define-tuple-type point (x y))

   (define/contract (point-move pt dir amount)
     (-> point? direction? real? point?)
     (match-define (point x y) pt)
     (match dir
       [(== up) (point x (+ y amount))]
       [(== down) (point x (- y amount))]
       [(== left) (point (- x amount) y)]
       [(== right) (point (+ x amount) y)])))
  
  (point-move (point 2 2) up 5)
  (point-move (point 1 4) left 10)
  (eval:error (point-move (point 1 4) 'up 5)))

@defform[
 (define-enum-type id (constant-id ...) enum-option ...)
 #:grammar ([enum-option
             (code:line #:predicate-name predicate-id)
             (code:line #:property-maker property-maker)])
 #:contracts ([property-maker
               (-> uninitialized-enum-descriptor?
                   (listof (cons/c struct-type-property? any/c)))])]{
 Creates an @tech{enum type} named @racket[id]. Each @racket[constant-id] is
 bound to a constant, and @racket[predicate-id] is bound to a predicate that
 returns @racket[#t] when given any of the constants and returns @racket[#f] for
 all other values. At compile-time, @racket[id] is bound to static information
 about the enum type which can be extracted with @racket[enum-id].

 If @racket[#:predicate-name] is not given, then @racket[predicate-id] defaults
 to @racket[id]@racketidfont{?}.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-enum-type suit (diamonds clubs hearts spades))
    (define-enum-type face (jack queen king))
    (define-tuple-type card (type suit))

    (define king-of-clubs (card king clubs)))

   (card-type king-of-clubs)
   (card-suit king-of-clubs)

   (eval:no-prompt
    (define/contract (card-value c)
      (-> card? (integer-in 1 13))
      (match (card-type c)
        [(? number? x) x]
        [(== jack) 11]
        [(== queen) 12]
        [(== king) 13])))

   (card-value king-of-clubs)
   (card-value (card 7 spades))
   (card-value (card jack hearts)))}

@defform[#:kind "provide transformer" (enum-out enum)]{
 Provides @racket[enum], which must be an @racket[enum-id], along with its
 predicate and constants.}

@section{Enum Type Information}

@defproc[(enum-type? [v any/c]) boolean?]{
 A predicate for @tech{enum types}.}

@defproc[(enum-type
          [name interned-symbol?]
          [constants keyset?]
          [#:predicate-name predicate-name (or/c interned-symbol? #f) #f]
          [#:discriminator-name discriminator-name (or/c interned-symbol? #f)
           #f]
          [#:selector-name selector-name (or/c interned-symbol? #f)])
         enum-type?]{
 Constructs an @tech{enum type} named @racket[name] and containing
 @racket[constants]. The optional @racket[predicate-name],
 @racket[discriminator-name], and @racket[selector-name] arguments control the
 result of @racket[object-name] on the functions implementing the type. They
 default to @racket[name]@racketidfont{?},
 @racketidfont{discriminator:}@racket[name], and
 @racketidfont{selector:}@racket[name] respectively. This function only
 constructs the information representing an enum type; to implement the type,
 use @racket[make-enum-implementation].

 @(examples
   #:eval (make-evaluator) #:once
   (enum-type 'weather (keyset #:sunny #:cloudy #:raining #:snowing)))}

@deftogether[[
 @defproc[(enum-type-name [type enum-type?]) interned-symbol?]
 @defproc[(enum-type-constants [type enum-type?]) keyset?]
 @defproc[(enum-type-predicate-name [type enum-type?]) interned-symbol?]
 @defproc[(enum-type-discriminator-name [type enum-type?]) interned-symbol?]
 @defproc[(enum-type-selector-name [type enum-type?]) interned-symbol?]]]{
 Accessors for the various components of an @tech{enum type}.}

@defproc[(enum-type-size [type enum-type?]) natural?]{
 Returns the number of enum constants in @racket[type].}

@section{Enum Type Descriptors}

Enum types are implemented by representing each constant in the enum with an
integer. The @tech{type descriptor} of an @tech{enum type} has contains two
functions for dynamically inspecting this mapping:

@itemlist[
 @item{An @deftech{enum discriminator} function, which maps an enum constant to
  an integer.}

 @item{An @deftech{enum selector} function, which returns the enum constant
  corresponding to a given integer.}]

These two functions are inverses of each other. Assuming the enum descriptor is
initialized, these functions can be used to dynamically convert between integers
and enum constants. To convert between strings or symbols and enum constants,
use the integers in conjunction with @racket[enum-type-constants]. Note that the
constants of an enum type are stored in a @tech{keyset}, so they are always in
alphabetical order.

@defproc[(enum-descriptor? [v any/c]) boolean?]{
 A predicate for enum @tech{type descriptors}.}

@defproc[(initialized-enum-descriptor? [v any/c]) boolean?]{
 A predicate for initialized enum @tech{type descriptors}.}

@defproc[(uninitialized-enum-descriptor? [v any/c]) boolean?]{
 A predicate for uninitialized enum @tech{type descriptors}.}

@defproc[(make-enum-implementation
          [type enum-type?]
          [#:inspector inspector inspector? (current-inspector)]
          [#:property-maker prop-maker
           (-> uninitialized-enum-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           default-enum-properties])
         initialized-enum-descriptor?]{
 Implements @racket[type] and returns a @tech{type descriptor} for the new
 implementation. The @racket[inspector] argument behaves the same as in
 @racket[make-struct-type], although there are no transparent or prefab enum
 types. The @racket[prop-maker] argument is similar to the corresponding
 argument of @racket[make-struct-implementation]. By default, enum types are
 created with properties that make them print like other named Racket constants
 such as @racket[eof] --- see @racket[default-enum-properties] for details.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define descriptor:weather
      (make-enum-implementation
       (enum-type 'weather (keyset #:sunny #:cloudy #:raining #:snowing))))
    (define weather? (enum-descriptor-predicate descriptor:weather))
    (define discriminator:weather
      (enum-descriptor-discriminator descriptor:weather))
    (define selector:weather (enum-descriptor-selector descriptor:weather)))

   (selector:weather 0)
   (selector:weather 1)

   (eval:no-prompt
    (define cloudy (selector:weather 0)))

   (weather? cloudy)
   (discriminator:weather cloudy))}

@defproc[(enum-descriptor-type [descriptor enum-descriptor?]) enum-type?]{
 Returns the @tech{enum type} that @racket[descriptor] implements.}

@defproc[(enum-descriptor-predicate [descriptor enum-descriptor?]) predicate/c]{
 Returns a predicate that returns true when given any enum constant created by
 @racket[descriptor]. The predicate is specific to @racket[descriptor] --- it
 will not return true for constants created by any other enum descriptors, even
 if they're different implementations of the same enum type as
 @racket[descriptor]. This is because enum types are @tech{nominal types}.}

@defproc[(enum-descriptor-selector [descriptor enum-descriptor?])
         (-> natural? (enum-descriptor-predicate descriptor))]{
 Returns the @tech{enum selector} of the enum type implementation represented by
 @racket[descriptor]. The selector accepts a nonnegative integer less than the
 number of enum constants in @racket[descriptor] and returns the corresponding
 constant.}

@defproc[(enum-descriptor-discriminator [descriptor enum-descriptor?])
         (-> (enum-descriptor-predicate descriptor) natural?)]{
 Returns the @tech{enum selector} of the enum type implementation represented by
 @racket[descriptor]. The selector accepts any enum constant created by
 @racket[descriptor] and returns that constant's integer representation.}

@defproc[(default-enum-properties [descriptor enum-descriptor?])
         (listof (cons/c struct-type-property? any/c))]{
 Returns implementations of @racket[prop:equal+hash], @racket[prop:object-name],
 and @racket[prop:custom-write] suitable for most @tech{enum types}. This
 function is called by @racket[make-enum-implementation] when no
 @racket[_prop-maker] argument is supplied.}

@defproc[(default-enum-equal+hash [descriptor enum-descriptor?]) equal+hash/c]{
 Builds an equality-checking function, a hashing function, and a secondary
 hashing function suitable for use with @racket[prop:equal+hash], each of which
 operate on enum constants created by @racket[descriptor]. Two constants are
 equal if and only if they are the same constant. This function is used by
 @racket[default-enum-properties] to implement @racket[prop:equal+hash].}

@defproc[(default-enum-custom-write [descriptor enum-descriptor?])
         custom-write-function/c]{
 Builds a @tech{custom write implementation} that prints enum constants created
 by @racket[descriptor] in a manner similar to built-in Racket constants, such
 as @racket[eof]. The printed representations include both the name of the enum
 type and the name of the enum constant. This function is used by
 @racket[default-enum-properties] to implement @racket[prop:custom-write].}

@defproc[(default-enum-object-name [descriptor enum-descriptor?])
         (or/c natural? (-> any/c any/c))]{
 Builds a value suitable for use with @racket[prop:object-name] that causes
 @racket[object-name] to return the name of the constant when used on enum
 constants created by @racket[descriptor]. This function is used by
 @racket[default-enum-properties] to implement @racket[prop:object-name].}

@section{Enum Type Bindings}
@defmodule[rebellion/type/enum/binding]

An @deftech{enum type binding} is a @tech{type binding} for an @tech{enum type}.
Enum type bindings contain compile-time information about the enum type's name
and values, as well as runtime bindings for its predicate,
@tech{type descriptor}, and other runtime components. To extract an enum type
binding bound by @racket[define-enum-type], use the @racket[enum-id]
@syntax-tech{syntax class}.

@defproc[(enum-binding? [v any/c]) boolean?]{
 A predicate for @tech{enum type bindings}.}

@defidform[#:kind "syntax class" enum-id]{
 A @syntax-tech{syntax class} for @tech{enum type bindings} bound by
 @racket[define-enum-type]. This class matches any @tech/reference{identifier}
 bound with @racket[define-syntax] to a value satisfying the
 @racket[enum-binding?] predicate, similar to the @racket[static] syntax class.
 Upon a successful match, the @racket[enum-id] class defines the following
 attributes:

 @itemlist[

 @item{@index-attribute[enum-id type] --- an attribute bound to a compile-time
   @racket[enum-type?] value describing the type.}

 @item{@index-attribute[enum-id name] --- a pattern variable bound to the enum
   type's name, as a quoted symbol.}

 @item{@index-attribute[enum-id constant ...] --- a pattern variable bound to
   the enum's constants.}

 @item{@index-attribute[enum-id constant-name ...] --- a pattern variable bound
   to the names of the enum's constants, as quoted symbols.}

 @item{@index-attribute[enum-id predicate] --- a pattern variable bound to the
   enum type's runtime type predicate.}

 @item{@index-attribute[enum-id discriminator] --- a pattern variable bound to
   the enum type's runtime @tech{enum discriminator}.}

 @item{@index-attribute[enum-id selector] --- a pattern variable bound to the
   enum type's runtime @tech{enum selector}.}

 @item{@index-attribute[enum-id descriptor] --- a pattern variable bound to the
   enum type's runtime @tech{type descriptor}.}]

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (require (for-syntax rebellion/type/enum/binding))
    
    (define-simple-macro (enum-names enum:enum-id)
      (list enum.constant-name ...))

    (define-enum-type compass-direction (north south east west)))

   (enum-names compass-direction))}

@defproc[(enum-binding-type [binding enum-binding?]) enum-type?]{
 Returns the @tech{enum type} that @racket[binding] is for. When an enum type
 binding is bound with @racket[define-syntax], this can be used at compile-time
 to obtain information about the size and names of the enum type.}

@defproc[(enum-binding-constants [binding enum-binding?]) (set/c identifier?)]{
 Returns a set of identifiers bound at runtime to the constants of the enum type
 bound by @racket[binding]. When an enum type binding is bound with
 @racket[define-syntax], this can be used by macros to generate a list of the
 enum's constants in code.}

@defproc[(enum-binding-descriptor [binding enum-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{type descriptor}
 for the enum type bound by @racket[binding]. When an enum type binding is bound
 with @racket[define-syntax], this can be used in macro-generated code to work
 with enum types dynamically.}

@defproc[(enum-binding-predicate [binding enum-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the predicate for the enum
 type bound by @racket[binding].}

@defproc[(enum-binding-selector [binding enum-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{enum selector} for
 the enum type bound by @racket[binding].}

@defproc[(enum-binding-discriminator [binding enum-binding?]) identifier?]{
 Returns an identifier that is bound at runtime to the @tech{enum discriminator}
 for the enum type bound by @racket[binding].}
