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


Collections may be mutable, immutable, or @deftech{unmodifiable}. A mutable collection can be changed
and updated by clients. Immutable collections never change: instead, operations on immutable
collections return modified copies of the original collection. Unmodifiable collections lie between
these two extremes: an unmodifiable collection cannot be changed by clients, but does not promise that
it will not change on its own. A typical use case for unmodifiable collections is for a module to
share access to a mutable collection with other modules without allowing other modules to modify the
collection: the module can wrap the mutable collection with an @deftech{unmodifiable view}, and share
that collection view with other modules. This provides other modules with read-only access to the
module's mutable state.


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
@include-section[(lib "rebellion/collection/sorted-set.scrbl")]
@include-section[(lib "rebellion/collection/range-set.scrbl")]
@include-section[(lib "rebellion/collection/keyset.scrbl")]
@include-section[(lib "rebellion/collection/record.scrbl")]
@include-section[(lib "rebellion/collection/table.scrbl")]
