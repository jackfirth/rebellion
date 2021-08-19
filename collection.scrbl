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
these two extremes. An unmodifiable collection cannot be changed by clients, but does not promise that
it will not change on its own.


Different collections need not necessarily have different state. A @tech{collection view} is a
collection whose implementation defers to the state of some other collection. Collection views can be
used for a wide variety of purposes, including efficiently operating on a subset of a collection,
constructing a thread-safe wrapper around a collection meant to be shared between threads, or adapting
one collection to the interface of another. Collection views come in a few main varieties:


@itemlist[
 @item{A @deftech{read-through view} reflects any changes in the underlying collection: updates to the
  viewed collection cause the view to change.}

 @item{A @deftech{write-through view} is a @tech{read-through view} that is mutable, and allows
  clients of the view to mutate the underlying collection by mutating the view.}

 @item{An @deftech{unmodifiable view} is a @tech{read-through view} that does not allow clients to
  mutate the view directly. Unmodifiable views are useful for sharing access to mutable state with
  clients who should be allowed to read that state, but not change it. @bold{Unmodifiable is not the
   same as immutable!} An unmodifiable view cannot be changed @emph{directly}, but may change
  @emph{indirectly} due to changes in the underlying collection.}]


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
