#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/symbol))

@title{Symbols}
@defmodule[rebellion/base/symbol]

@defproc[(interned-symbol? [v any/c]) boolean?]

@defproc[(unreadable-symbol? [v any/c]) boolean?]

@defproc[(unininterned-symbol? [v any/c]) boolean?]
