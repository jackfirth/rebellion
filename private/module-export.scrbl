#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/set
                     rebellion/module-export)
          rebellion/private/scribble-evaluator-factory
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/module-export)
    #:private (list 'racket/base)))

@title{Module Exports}
@defmodule[rebellion/module-export]

@defproc[(module-exports [modpath module-path?]) (set/c export? #:cmp 'equal)]{
 Loads @racket[modpath] and returns its exports. This function is a convenience
 wrapper around @racket[module->exports].

 @(examples
   #:eval (make-evaluator) #:once
   (module-exports 'racket/bool))}

@defproc[(export? [v any/c]) boolean?]{
 A predicate for module exports returned by @racket[module-exports].}

@defproc[(variable-export? [v any/c]) boolean?]{
 A predicate for variable exports. Implies @racket[export?].}

@defproc[(syntax-export? [v any/c]) boolean?]{
 A predicate for syntax exports. Implies @racket[export?].}

@deftogether[[
 @defproc[(export-name [export export?]) symbol?]
 @defproc[(export-phase [export export?]) (or/c exact-integer? #f)]
 @defproc[(export-origins [export export?])
          (set/c export-origin? #:cmp 'equal)]]]{
 Accessors for a module export's name, phase, and set of origins (for bindings
 that were required from other modules and then re-provided).}

@defproc[(export-origin? [v any/c]) boolean?]{
 A predicate for module export origin information.}

@deftogether[[
 @defproc[(export-origin-source-module [origin export-origin?])
          module-path-index?]
 @defproc[(export-origin-phase [origin export-origin?])
          (or/c exact-integer? #f)]
 @defproc[(export-origin-phase-shift [origin export-origin?])
          (or/c exact-integer? #f)]
 @defproc[(export-origin-imported-alias [origin export-origin?])
          (or/c symbol? #f)]]]{
 Accessors for the various fields of a module export origin information value.}
