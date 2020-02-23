#lang scribble/manual

@(require (for-label rebellion))

@title{Rebellion}
@defmodule[rebellion]

Rebellion is a collection of core Racket libraries that includes a stream
processing system built on @tech{transducers} and @tech{reducers}, new kinds of
collections such as @tech{multisets} and @tech{multidicts}, a suite of libraries
for defining new @racket[struct]-based types including @tech{record types} and
@tech{enum types}, and much more. The goal of Rebellion is to make high quality
standard libraries accessible to all Racketeers regardless of what @hash-lang
they're using.

@table-of-contents[]

@include-section[(lib "rebellion/private/base.scrbl")]
@include-section[(lib "rebellion/private/type.scrbl")]
@include-section[(lib "rebellion/private/streaming.scrbl")]
@include-section[(lib "rebellion/private/collection.scrbl")]
@include-section[(lib "rebellion/private/concurrency.scrbl")]
@include-section[(lib "rebellion/private/binary.scrbl")]
@include-section[(lib "rebellion/private/module.scrbl")]

@include-section[(lib "rebellion/private/custom-write.scrbl")]
@include-section[(lib "rebellion/private/equal+hash.scrbl")]
@include-section[(lib "rebellion/private/media.scrbl")]
@include-section[(lib "rebellion/private/octet-stream.scrbl")]
@include-section[(lib "rebellion/private/permutation.scrbl")]
@include-section[(lib "rebellion/private/point.scrbl")]
@include-section[(lib "rebellion/private/text.scrbl")]
@include-section[(lib "rebellion/private/web-graph.scrbl")]
@include-section[(lib "rebellion/private/web-link.scrbl")]
