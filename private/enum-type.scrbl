#lang scribble/manual

@(require (for-syntax racket/base
                      racket/symbol)
          (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     racket/match
                     rebellion/base/symbol
                     rebellion/type/enum
                     rebellion/type/enum/binding
                     rebellion/type/tuple
                     syntax/parse
                     syntax/parse/define)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example
          syntax/parse/define)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
                   'racket/match
                   'rebellion/type/enum
                   'rebellion/type/tuple
                   'syntax/parse/define)
    #:private (list 'racket/base)))

@; This is used to document the attributes of syntax classes. It indexes them
@; in Scribble as class.attribute, so they can be searched.
@(define-simple-macro
   (index-attribute class-id:id
                    attribute-id:id
                    (~and ellipsis (~literal ...)) ...)
   #:with attribute-string (symbol->immutable-string (syntax-e #'attribute-id))
   #:with (ellipses-string ...)
   (for/list ([_ (in-range (length (syntax->list #'(ellipsis ...))))])
     #`'" ...")
   #:with indexed-word
   (format "~a.~a" (syntax-e #'class-id) (syntax-e #'attribute-id))
   (index* (list 'indexed-word)
           (list (racketidfont 'indexed-word))
           (racketidfont 'attribute-string ellipses-string ...)))

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

@defproc[(enum-descriptor? [v any/c]) boolean?]{
 A predicate for enum @tech{type descriptors}.}

@defproc[(initialized-enum-descriptor? [v any/c]) boolean?]{
 A predicate for initialized enum @tech{type descriptors}.}

@defproc[(uninitialized-enum-descriptor? [v any/c]) boolean?]{
 A predicate for uninitialized enum @tech{type descriptors}.}

@section{Enum Type Bindings}
@defmodule[rebellion/type/enum/binding]

An @deftech{enum type binding} is a @tech{type binding} for @tech{enum types}.
Enum type bindings contain compile-time information about the enum type's name
and values, as well as runtime bindings for its predicate and
@tech{type descriptor}. To extract an enum type binding bound by
@racket[define-enum-type], use the @racket[enum-id] @syntax-tech{syntax class}.

@defidform[#:kind "syntax class" enum-id]{
 A @syntax-tech{syntax class} for @tech{enum type bindings} bound by
 @racket[define-enum-type]. This class matches any @tech/reference{identifier}
 bound with @racket[define-syntax] to a value satisfying the
 @racket[enum-type-binding?] predicate, similar to the @racket[static] syntax
 class. Upon a successful match, the @racket[enum-id] class defines the
 following attributes:

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
