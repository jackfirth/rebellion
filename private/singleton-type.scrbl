#lang scribble/manual

@(require (for-label racket/base
                     rebellion/type/singleton)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/examples)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/type/singleton)
    #:private (list 'racket/base)))

@title{Singletons}
@defmodule[rebellion/type/singleton]

@defproc[(singleton-type? [v any/c]) boolean?]

@defproc[(singleton-type
          [name interned-symbol?]
          [#:predicate-name pred-name (or/c interned-symbol? #f) #f])
         singleton-type?]

@defproc[(singleton-type-name [type singleton-type?]) interned-symbol?]

@defproc[(singleton-type-predicate-name [type singleton-type?])
         interned-symbol?]

@defproc[(make-singleton-type
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


@defform[
 (define-singleton-type id singleton-option ...)
 #:grammar ([singleton-option
             (code:line #:name name)
             (code:line #:predicate-name predicate-name)
             (code:line #:descriptor-name descriptor-name)
             (code:line #:type-representation-name type-representation-name)
             (code:line #:inspector inspector)
             (code:line #:property-maker property-maker)])
 #:contracts ([inspector inspector?]
              [property-maker
               (-> uninitialized-singleton-descriptor?
                   (listof (cons/c struct-type-property? any/c)))])]{
 Defines a @tech{singleton type} and binds the following identifiers:

 @itemlist[

 @item{@racket[name] (which defaults to @racket[id]), the singleton instance.}

 @item{@racket[predicate-name] (which defaults to @racket[id]@racketidfont{?}),
   a predicate that returns @racket[#t] for the singleton instance and false for
   all other values.}

 @item{@racket[descriptor-name] (which defaults to
   @racketidfont{descriptor:}@racket[id]), a @tech{singleton type descriptor}
   for the defined singleton type.}

 @item{@racket[type-representation-name] (which defaults to
   @racketidfont{type:}@racket[id]), the defined @tech{singleton type}.}]

 @(examples
   #:eval (make-evaluator) #:once
   (define-singleton-type infinity)
   
   infinity
   (infinity? infinity)
   descriptor:infinity
   type:infinity)}

@defproc[(make-default-singleton-properties [descriptor singleton-descriptor?])
         (listof (cons/c struct-type-property? any/c))]
