#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/record
                     rebellion/record/field)
          rebellion/private/scribble-evaluator-factory
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/record/field)
    #:private (list 'racket/base)))

@title{Record Fields}
@defmodule[rebellion/record/field]

@defproc[(field? [v any/c]) boolean?]{
 A predicate for @tech{record fields}.}

@defproc[(field [#:<kw> v any/c]) field?]{
 Constructs a @tech{record field} mapping whatever keyword is given for
 @racket[#:<kw>] to @racket[v].

 @(examples
   #:eval (make-evaluator) #:once
   (field #:title "Fabulous Widget 2.0")
   (field #:color 'firetruck-red))}

@deftogether[[
 @defproc[(field-name [field field?]) keyword?]
 @defproc[(field-value [field field?]) any/c]]]{
 Accessors for the name and value of a @tech{record field}.

 @(examples
   #:eval (make-evaluator) #:once
   (define fld (field #:title "Fabulous Widget 2.0"))
   (field-name fld)
   (field-value fld))}
