#lang scribble/manual

@(require (for-label rebellion/collection)
          (submod rebellion/private/scribble-cross-document-tech doc))

@title[#:style (list 'toc)]{Collections}
@defmodule[rebellion/collection]

Rebellion provides several different types of collections. A @deftech{
 collection} is a container for values, like a @tech/reference{list} or a
@tech/reference{hash table}. Different types of collections have different
properties, for example @tech/reference{sets} are unordered and have no
duplicate elements. For advice on how to choose the right collection type, see
@secref["choosing-collections"].

@local-table-of-contents[]

@include-section[(lib "rebellion/private/choosing-collections.scrbl")]
@include-section[(lib "rebellion/private/entry.scrbl")]
@include-section[(lib "rebellion/private/list.scrbl")]
@include-section[(lib "rebellion/private/vector.scrbl")]
@include-section[(lib "rebellion/private/vector-builder.scrbl")]
@include-section[(lib "rebellion/private/immutable-vector.scrbl")]
@include-section[(lib "rebellion/private/set.scrbl")]
@include-section[(lib "rebellion/private/hash.scrbl")]
@include-section[(lib "rebellion/private/multiset.scrbl")]
@include-section[(lib "rebellion/private/multidict.scrbl")]
@include-section[(lib "rebellion/private/association-list.scrbl")]
@include-section[(lib "rebellion/private/keyset.scrbl")]
@include-section[(lib "rebellion/private/record.scrbl")]
@include-section[(lib "rebellion/private/table.scrbl")]
