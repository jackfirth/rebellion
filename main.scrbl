#lang scribble/manual

@(require (for-label rebellion))

@title{Rebellion}
@defmodule[rebellion]

Rebellion is a set of infrastructure libraries for Racketeers to build new
languages, new frameworks, and new tools with.

@table-of-contents[]

@include-section[(lib "rebellion/private/base.scrbl")]
@include-section[(lib "rebellion/private/binary.scrbl")]
@include-section[(lib "rebellion/private/collection.scrbl")]
@include-section[(lib "rebellion/private/custom-write.scrbl")]
@include-section[(lib "rebellion/private/equal+hash.scrbl")]
@include-section[(lib "rebellion/private/immutable-string.scrbl")]
@include-section[(lib "rebellion/private/media.scrbl")]
@include-section[(lib "rebellion/private/module-export.scrbl")]
@include-section[(lib "rebellion/private/octet-stream.scrbl")]
@include-section[(lib "rebellion/private/pair.scrbl")]
@include-section[(lib "rebellion/private/permutation.scrbl")]
@include-section[(lib "rebellion/private/point.scrbl")]
@include-section[(lib "rebellion/private/result.scrbl")]
@include-section[(lib "rebellion/private/text.scrbl")]
@include-section[(lib "rebellion/private/type.scrbl")]
@include-section[(lib "rebellion/private/variant.scrbl")]
@include-section[(lib "rebellion/private/web-graph.scrbl")]
@include-section[(lib "rebellion/private/web-link.scrbl")]
