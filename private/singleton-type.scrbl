#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/contract/region
                     rebellion/base/symbol
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
             (code:line #:value-name value-id)
             (code:line #:predicate-name predicate-id)
             (code:line #:inspector inspector)
             (code:line #:property-maker property-maker)])
 #:contracts ([inspector inspector?]
              [property-maker
               (-> uninitialized-singleton-descriptor?
                   (listof (cons/c struct-type-property? any/c)))])]{
 Creates a @tech{singleton type} and binds the following identifiers:

 @itemlist[
 @item{@racket[value-id], which defaults to @racket[id] --- the unique
   instance of the created type.}

 @item{@racket[predicate-id], which defaults to @racket[id]@racketidfont{?} ---
   a predicate that returns @racket[#t] when given the singleton instance and
   false for all other values.}]

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define-singleton-type infinity))

   infinity
   (infinity? infinity))}

@defproc[(singleton-type? [v any/c]) boolean?]

@defproc[(singleton-type
          [name interned-symbol?]
          [#:predicate-name pred-name (or/c interned-symbol? #f) #f])
         singleton-type?]

@defproc[(singleton-type-name [type singleton-type?]) interned-symbol?]

@defproc[(singleton-type-predicate-name [type singleton-type?])
         interned-symbol?]

@defproc[(make-singleton-implementation
          [type singleton-type?]
          [#:inspector inspector inspector? (current-inspector)]
          [#:property-maker prop-maker
           (-> uninitialized-singleton-descriptor?
               (listof (cons/c struct-type-property? any/c)))
           make-default-singleton-properties])
         initialized-singleton-descriptor?]

@defproc[(singleton-descriptor? [v any/c]) boolean?]

@defproc[(initialized-singleton-descriptor? [v any/c]) boolean?]

@defproc[(uninitialized-singleton-descriptor? [v any/c]) boolean?]

@defproc[(singleton-descriptor-predicate [descriptor singleton-descriptor?])
         predicate/c]

@defproc[(singleton-descriptor-instance
          [descriptor initialized-singleton-descriptor?])
         (singleton-descriptor-predicate descriptor)]

@defproc[(make-default-singleton-properties [descriptor singleton-descriptor?])
         (listof (cons/c struct-type-property? any/c))]
