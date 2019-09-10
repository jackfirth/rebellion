#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     rebellion/base/symbol
                     rebellion/custom-write
                     rebellion/equal+hash
                     rebellion/type/wrapper)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/contract/base
                   'racket/contract/region
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
  #:eval (make-evaluator) #:once #:label #f
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
 #:grammar ([option (code:line #:constructor-name constructor-id)
             (code:line #:accessor-name accessor-id)
             (code:line #:predicate-name predicate-id)
             (code:line #:property-maker prop-maker-expr)])
 #:contracts ([prop-maker-expr
               (-> uninitialized-wrapper-descriptor?
                   (listof (cons/c struct-type-property? any/c)))])]{
 Creates a new @tech{wrapper type} named @racket[id] and binds the following
 identifiers:

 @itemlist[
 @item{@racket[constructor-id], which defaults to @racket[id] --- a constructor
   function that wraps a value and returns an instance of the created type.}

 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate function that returns @racket[#t] when given instances of the
   created type and returns @racket[#f] otherwise.}

 @item{@racket[accessor-id], which defaults to @racket[id]@racketidfont{-value}
   --- an accessor function that unwraps instances of the created type and
   returns their underlying value.}]

 @(examples
   #:eval (make-evaluator) #:once
   (define-wrapper-type seconds)
   (seconds 10)
   (seconds-value (seconds 25))
   (seconds? (seconds 10)))}

@defproc[(wrapper-type? [v any/c]) boolean?]

@defproc[(wrapper-type
          [name interned-symbol?]
          [#:predicate-name predicate-name (or/c interned-symbol? #f) #f]
          [#:constructor-name constructor-name (or/c interned-symbol? #f) #f]
          [#:accessor-name accessor-name (or/c interned-symbol? #f) #f])
         wrapper-type?]

@deftogether[[
 @defproc[(wrapper-type-name [type wrapper-type?]) interned-symbol?]
 @defproc[(wrapper-type-predicate-name [type wrapper-type?]) interned-symbol?]
 @defproc[(wrapper-type-constructor-name [type wrapper-type?]) interned-symbol?]
 @defproc[(wrapper-type-accessor-name [type wrapper-type?]) interned-symbol?]]]

@section{Wrapper Descriptors}

@defproc[(wrapper-descriptor? [v any/c]) boolean?]

@defproc[(initialized-wrapper-descriptor? [v any/c]) boolean?]

@defproc[(uninitialized-wrapper-descriptor? [v any/c]) boolean?]

@defproc[(make-wrapper-implementation
          [type wrapper-type?]
          [#:property-maker prop-maker
           (-> uninitialized-wrapper-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           make-default-wrapper-properties]
          [#:inspector inspector inspector? (current-inspector)])
         initialized-wrapper-descriptor?]

@defproc[(wrapper-descriptor-type [descriptor wrapper-descriptor?])
         wrapper-type?]

@defproc[(wrapper-descriptor-predicate [descriptor wrapper-descriptor?])
         predicate/c]

@defproc[(wrapper-descriptor-constructor [descriptor wrapper-descriptor?])
         (-> any/c (wrapper-descriptor-predicate descriptor))]

@defproc[(wrapper-descriptor-accessor [descriptor wrapper-descriptor?])
         (-> (wrapper-descriptor-predicate descriptor) any/c)]

@section{Wrapper Type Properties}

@defproc[(make-default-wrapper-properties [descriptor wrapper-descriptor?])
         (listof (cons/c struct-type-property? any/c))]

@defproc[(make-default-wrapper-equal+hash [descriptor wrapper-descriptor?])
         equal+hash/c]

@defproc[(make-default-wrapper-custom-write [descriptor wrapper-descriptor?])
         custom-write-function/c]

@section{Static Wrapper Bindings}

@defproc[(wrapper-binding? [v any/c]) boolean?]

@defproc[(wrapper-binding [#:type type wrapper-type?]
                          [#:descriptor-id descriptor identifier?]
                          [#:predicate-id predicate identifier?]
                          [#:constructor-id constructor identifier?])
         wrapper-binding?]

@defproc[(wrapper-binding-type [binding wrapper-binding?]) wrapper-type?]

@defproc[(wrapper-binding-descriptor-id [binding wrapper-binding?]) identifier?]

@defproc[(wrapper-binding-predicate-id [binding wrapper-binding?]) identifier?]

@defproc[(wrapper-binding-constructor-id [binding wrapper-binding?])
         identifier?]

@defproc[(wrapper-binding-accessor-id [binding wrapper-binding?]) identifier?]

@defidform[#:kind "syntax class" wrapper-type-id]{
 A syntax class that matches an identifier bound with @racket[define-syntax] to
 a @racket[wrapper-binding?] value.}
