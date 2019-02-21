#lang scribble/manual
@(require (for-label rebellion))

@title{Rebellion}
@defmodule[rebellion]

Rebellion is a set of infrastructure libraries for Racketeers to build new
languages, new frameworks, and new tools with.

@local-table-of-contents[#:style 'immediate-only]

@include-section[(lib "rebellion/private/generative-token.scrbl")]
@include-section[(lib "rebellion/private/record.scrbl")]
@include-section[(lib "rebellion/private/table.scrbl")]
@include-section[(lib "rebellion/private/variant.scrbl")]
