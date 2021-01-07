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

@include-section[(lib "rebellion/collection/choosing-collections.scrbl")]
@include-section[(lib "rebellion/collection/entry.scrbl")]
@include-section[(lib "rebellion/collection/list.scrbl")]
@include-section[(lib "rebellion/collection/vector.scrbl")]
@include-section[(lib "rebellion/collection/vector/builder.scrbl")]
@include-section[(lib "rebellion/collection/immutable-vector.scrbl")]
@include-section[(lib "rebellion/collection/set.scrbl")]
@include-section[(lib "rebellion/collection/hash.scrbl")]
@include-section[(lib "rebellion/collection/multiset.scrbl")]
@include-section[(lib "rebellion/collection/multidict.scrbl")]
@include-section[(lib "rebellion/collection/association-list.scrbl")]
@include-section[(lib "rebellion/collection/keyset.scrbl")]
@include-section[(lib "rebellion/collection/record.scrbl")]
@include-section[(lib "rebellion/collection/table.scrbl")]
