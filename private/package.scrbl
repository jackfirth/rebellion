#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/package
                     rebellion/immutable-string))

@title{Packages}
@defmodule[rebellion/package]

@defproc[(package? [v any/c]) boolean?]

@defproc[(package [#:name name immutable-string?]
                  [#:short-name short-name interned-symbol?]
                  [#:catalog catalog immutable-string?])
         package?]

@defproc[(package-name [pkg package?]) immutable-string?]
@defproc[(package-short-name [pkg package?]) interned-symbol?]
@defproc[(package-catalog [pkg package?]) immutable-string?]

@defproc[(get-catalog-packages [catalog immutable-string?])
         (immutable-set/c package?)]

@defthing[plt-package-catalog immutable-string?]
@defthing[planet-compat-package-catalog immutable-string?]
