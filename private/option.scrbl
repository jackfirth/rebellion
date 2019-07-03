#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/option)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/option)
    #:private (list 'racket/base)))

@title{Option Values}
@defmodule[rebellion/base/option]

@defproc[(option? [v any/c]) boolean?]

@defproc[(present? [v any/c]) boolean?]

@defproc[(present [v any/c]) present?]

@defproc[(present-value [pres present?]) any/c]

@defproc[(absent? [v any/c]) boolean?]

@defthing[absent absent?]

@defproc[(option-case [opt option?]
                      [#:present present-handler (-> any/c any/c)]
                      [#:absent absent-handler (-> any/c)])
         any/c]

@defproc[(option-map [opt option?] [f (-> any/c any/c)]) option?]

@defproc[(option-flat-map [opt option?] [f (-> any/c option?)]) option?]

@defproc[(option-filter [opt option?] [pred predicate/c]) option?]

@defproc[(option-get [opt option?] [default any/c]) any/c]
