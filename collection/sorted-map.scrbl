#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     rebellion/base/comparator
                     rebellion/base/option
                     rebellion/base/range
                     rebellion/collection/entry
                     rebellion/collection/sorted-map
                     rebellion/concurrency/lock
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/base/comparator
                   'rebellion/collection/sorted-map)
    #:private (list 'racket/base)))


@title{Sorted Maps}
@defmodule[rebellion/collection/sorted-map]


A @deftech{sorted map} is a @tech{collection} of key-value mappings whose keys are unique and sorted
according to some @tech{comparator}. Sorted maps may be mutable, immutable, or @tech{unmodifiable}.
Two immutable sorted maps are @racket[equal?] if they contain the same entries and use @racket[equal?]
comparators. Two mutable sorted maps are @racket[equal?] if they will @emph{always} contain the same
entries and use @racket[equal?] comparators, meaning that they share the same mutable state. This is
not necessarily the same as being @racket[eq?], as some sorted maps may be views of others.

All sorted maps are @tech/reference{sequences}. When iterated, a sorted map traverses its entries in
ascending order as defined by its comparator. To traverse a sorted map in descending order, either use
@racket[in-sorted-map] with @racket[#:descending?] set to true, or reverse the sorted map with
@racket[sorted-map-reverse]. Note that @racket[sorted-map-reverse] returns a view of the original map,
not a copy, so it constructs the view in constant time regardless of the size of the original map.


@defproc[(sorted-map? [v any/c]) boolean?]{
 A predicate for @tech{sorted maps}. Includes mutable, immutable, and @tech{unmodifiable} sorted
 maps.}


@defproc[(mutable-sorted-map? [v any/c]) boolean?]{
 A predicate for mutable @tech{sorted maps}. Implies @racket[sorted-map?].}


@defproc[(immutable-sorted-map? [v any/c]) boolean?]{
 A predicate for immutable @tech{sorted maps}. Implies @racket[sorted-map?].}


@section{Constructing Sorted Maps}


@defproc[(sorted-map [key any/c] [value any/c] ... ... [#:key-comparator key-comparator comparator?])
         immutable-sorted-map?]{

 Constructs an immutable @tech{sorted map} mapping each @racket[key] to the corresponding
 @racket[value], where keys are sorted by @racket[key-comparator]. The input @racket[key]s may be
 given in any order. Duplicate keys (as in, keys that @racket[key-comparator] considers equivalent)
 are disallowed and result in a contract error.

 @(examples
   #:eval (make-evaluator) #:once
   (sorted-map 3 'c 1 'a 4 'd 2 'b 5 'e #:key-comparator natural<=>))}


@section{Iterating Sorted Maps}


@defproc[(in-sorted-map [map sorted-map?] [#:descending? descending? boolean? #false])
         (sequence/c entry?)]{

 Returns a @tech/reference{sequence} that iterates through the entries of @racket[map] in ascending
 order. If @racket[descending?] is true, the sequence iterates in descending order instead.}


@defproc[(in-sorted-map-keys [map sorted-map?] [#:descending? descending? boolean? #false])
         (sequence/c any/c)]{

 Returns a @tech/reference{sequence} that iterates through the keys of @racket[map] in ascending
 order. If @racket[descending?] is true, the sequence iterates in descending order instead.}


@defproc[(in-sorted-map-values [map sorted-map?] [#:descending? descending? boolean? #false])
         (sequence/c any/c)]{

 Returns a @tech/reference{sequence} that iterates through the values of @racket[map] in ascending
 order by key. If @racket[descending?] is true, the sequence iterates in descending order instead.}


@section{Querying Sorted Maps}


@defproc[(sorted-map-empty? [map sorted-map?]) boolean?]{

 Returns true if @racket[map] contains no entries, returns false otherwise. Note that this operation
 can be combined with @racket[sorted-submap] to efficiently determine if a range within a sorted map
 is empty.}


@defproc[(sorted-map-size [map sorted-map?]) natural?]{

 Returns the number of entries in @racket[map]. Note that this operation can be combined with
 @racket[sorted-submap] to efficiently determine how many elements are contained within a range of a
 sorted map.}


@defproc[(sorted-map-key-comparator [map sorted-map?]) comparator?]{

 Returns the @tech{comparator} used by @racket[map] to sort keys.}


@defproc[(sorted-map-contains-key? [map sorted-map?] [key any/c]) boolean?]{

 Returns true if @racket[map] contains an entry for @racket[key], returns false otherwise.}


@defproc[(sorted-map-contains-value? [map sorted-map?] [value any/c]) boolean?]{

 Returns true if @racket[map] maps at least one key to @racket[value], returns false otherwise.}


@defproc[(sorted-map-contains-entry? [map sorted-map?] [entry entry?]) boolean?]{

 Returns true if @racket[map] contains @racket[entry].}


@defproc[(sorted-map-get
          [map sorted-map?]
          [key any/c]
          [failure-result failure-result/c (λ () (raise ...))])
         any/c]{

 Returns the value mapped by @racket[key] in @racket[map]. If no value exists for @racket[key], then
 @racket[failure-result] determines the result: if it's a procedure it's called with no arguments
 to produce the result, if it's not a procedure it's returned directly.}


@defproc[(sorted-map-get-option [map sorted-map?] [key any/c]) option?]{

 Returns the value mapped by @racket[key] in @racket[map], or @racket[absent] if no value exists for
 @racket[key].}


@defproc[(sorted-map-get-entry
          [map sorted-map?] [key any/c] [failure-result failure-result/c (λ () (raise ...))])
         entry?]{

 Returns the entry for @racket[key] in @racket[map]. If no value exists for @racket[key], then
 @racket[failure-result] determines the resulting entry's value: if it's a procedure it's called with
 no arguments to produce the value, if it's not a procedure it's used directly as the resulting
 entry's value.}


@defproc[(sorted-map-least-key [map sorted-map?]) option?]{

 Returns the first and smallest key in @racket[map], or @racket[absent] if @racket[map] is empty.}


@defproc[(sorted-map-greatest-key [map sorted-map?]) option?]{

 Returns the last and largest key in @racket[map], or @racket[absent] if @racket[map] is empty.}


@defproc[(sorted-map-key-less-than [map sorted-map?] [upper-bound any/c]) option?]{

 Returns the largest key in @racket[map] less than @racket[upper-bound], or @racket[absent] if no such
 key exists.}


@defproc[(sorted-map-key-greater-than [map sorted-map?] [lower-bound any/c]) option?]{

 Returns the smallest key in @racket[map] greater than @racket[lower-bound], or @racket[absent] if no
 such key exists.}


@defproc[(sorted-map-key-at-most [map sorted-map?] [upper-bound any/c]) option?]{

 Returns the largest key in @racket[map] less than or equivalent to @racket[upper-bound], or
 @racket[absent] if no such key exists.}


@defproc[(sorted-map-key-at-least [map sorted-map?] [lower-bound any/c]) option?]{

 Returns the smallest key in @racket[map] greater than or equivalent to @racket[lower-bound], or
 @racket[absent] if no such key exists.}


@defproc[(sorted-map-least-entry [map sorted-map?]) (option/c entry?)]{

 Returns the entry for the first and smallest key in @racket[map], or absent if @racket[map] is
 empty.}


@defproc[(sorted-map-greatest-entry [map sorted-map?]) (option/c entry?)]{

 Returns the entry for the last and largest key in @racket[map], or @racket[absent] if @racket[map] is
 empty.}


@defproc[(sorted-map-entry-less-than [map sorted-map?] [upper-key-bound any/c]) (option/c entry?)]{

 Returns the entry for the largest key in @racket[map] less than @racket[upper-key-bound], or
 @racket[absent] if no such key exists.}


@defproc[(sorted-map-entry-greater-than [map sorted-map?] [lower-key-bound any/c]) (option/c entry?)]{

 Returns the entry for the smallest key in @racket[map] greater than @racket[lower-key-bound], or
 @racket[absent] if no such key exists.}


@defproc[(sorted-map-entry-at-most [map sorted-map?] [upper-key-bound any/c]) (option/c entry?)]{

 Returns the entry for the largest key in @racket[map] less than or equivalent to
 @racket[upper-key-bound], or @racket[absent] if no such key exists.}


@defproc[(sorted-map-entry-at-least [map sorted-map?] [lower-key-bound any/c]) (option/c entry?)]{

 Returns the entry for the smallest key in @racket[map] greater than or equivalent to
 @racket[lower-key-bound], or @racket[absent] if no such key exists.}


@section{Sorted Map Views}


@defproc[(sorted-submap [map sorted-map?] [key-range range?]) sorted-map?]{

 Returns a view of the entries in @racket[map] with keys that fall within @racket[key-range].
 @bold{The returned submap is not a copy!} It is a @tech{read-through view} of @racket[map], and any
 modifications to @racket[map] will be reflected in the returned view. The returned view is an
 @racket[immutable-sorted-map?] if @racket[map] is immutable, and similarly it is a
 @racket[mutable-sorted-map?] if @racket[map] is mutable.

 When used on mutable sorted maps, the returned map is also a @tech{write-through view} --- mutating
 the returned submap will mutate the original, underlying map. The returned submap supports all of the
 same operations as ordinary mutable sorted maps, with the exception that inserting keys outside
 @racket[key-range] is disallowed. Additionally, note that calling @racket[sorted-map-remove!] on the
 submap view with a key outside @racket[key-range] will have no effect on either the submap view
 @emph{or} the original map, as @racket[sorted-map-remove!] does nothing on maps that do not contain
 an entry for the key being removed.}


@defproc[(sorted-map-reverse [map sorted-map?]) sorted-map?]{

 Returns a view of @racket[map] that sorts keys in the opposite order.
 @bold{The returned map is not a copy!} It is a @tech{read-through view} of @racket[map], and any
 modifications to @racket[map] will be reflected in the returned view. The returned view is an
 @racket[immutable-sorted-map?] if @racket[map] is immutable, and similarly it is a
 @racket[mutable-sorted-map?] if @racket[map] is mutable. Note that calling
 @racket[sorted-map-key-comparator] on the returned view returns a reversed version of the comparator
 on @racket[map].

 When used on mutable sorted maps, the returned map is also a @tech{write-through view} --- mutating
 the returned map will mutate the original, underlying map. The returned map supports all of the same
 operations as ordinary mutable sorted maps.}


@defproc[(sorted-map-keys [map sorted-map?]) sorted-set?]{

 Returns a view of the keys in @racket[map]. @bold{The returned set is not a copy!} It is a
 @tech{read-through view} of @racket[map], and modifications to @racket[map] will be reflected in the
 returned view. The returned view is an @racket[immutable-sorted-set?] if @racket[map] is immutable,
 and similarly it is a @racket[mutable-sorted-set?] if @racket[map] is mutable.

 When used on mutable sorted maps, the returned set is also a @tech{write-through view} --- mutating
 the returned key set will mutate the original, underlying map. The returned key set only supports set
 removal operations and cannot be used to insert new entries into @racket[map].}


@defproc[(sorted-map-entries [map sorted-map?]) sorted-set?]{

 Returns a view of the entries in @racket[map]. @bold{The returned set is not a copy!} It is a
 @tech{read-through view} of @racket[map], and modifications to @racket[map] will be reflected in the
 returned view. The returned view is an @racket[immutable-sorted-set?] if @racket[map] is immutable,
 and similarly it is a @racket[mutable-sorted-set?] if @racket[map] is mutable. The returned set uses
 a @tech{comparator} on @tech{entries} that ignores the entry's value and compares its keys using the
 same comparator as @racket[map].

 When used on mutable sorted maps, the returned set is also a @tech{write-through view} --- mutating
 the returned entry set will mutate the original, underlying map. The returned set supports all of the
 same operations as ordinary mutable sorted sets. Note that because it uses a comparator that ignores
 entry values, it cannot be used to insert entries with duplicate keys.}


@section{Modifying Sorted Maps}


@defproc[(sorted-map-get! [map mutable-sorted-map?] [key any/c] [failure-result failure-result/c])
         any/c]{

 Returns the value mapped by @racket[key] in @racket[map]. If no value exists for @racket[key], then
 @racket[failure-result] determines the result: if it's a procedure it's called with no arguments
 to produce the result, if it's not a procedure it's returned directly. Either way, the returned
 result is also inserted into @racket[map] as the value for @racket[key].}


@defproc[(sorted-map-get-entry! [map sorted-map?] [key any/c] [failure-result failure-result/c])
         entry?]{

 Returns the entry for @racket[key] in @racket[map]. If no value exists for @racket[key], then
 @racket[failure-result] determines the resulting entry's value: if it's a procedure it's called with
 no arguments to produce the value, if it's not a procedure it's used directly as the resulting
 entry's value. Either way, the produced value is also inserted into @racket[map] as the value for
 @racket[key].}


@defproc[(sorted-map-put [map immutable-sorted-map?] [key any/c] [value any/c])
         immutable-sorted-map?]{

 Functionally inserts a mapping from @racket[key] to @racket[value] into @racket[map] by returning a
 new immutable sorted map containing all of the entries of @racket[map] and the additional inserted
 mapping from @racket[key] to @racket[value]. The input map is not modified.}


@defproc[(sorted-map-put! [map mutable-sorted-map?] [key any/c] [value any/c]) void?]{

 Inserts a mapping from @racket[key] to @racket[value] into @racket[map], overwriting any existing
 mapping for @racket[key] in @racket[map].}


@defproc[(sorted-map-put-all [map immutable-sorted-map?] [entries (sequence/c entry?)])
         immutable-sorted-map?]{

 Functionally inserts a mapping for each key-value entry in @racket[entries] into @racket[map] by
 returning a new immutable sorted map containing all of the entries of @racket[map] and the additional
 inserted mappings. If any @racket[entries] have duplicate keys, a contract error is raised.}


@defproc[(sorted-map-put-all! [map mutable-sorted-map?] [entries (sequence/c entry?)]) void?]{

 Inserts a mapping for each key-value entry in @racket[entries] into @racket[map]. If any
 @racket[entries] have duplicate keys, a contract error is raised.}


@defproc[(sorted-map-put-if-absent [map immutable-sorted-map?] [key any/c] [value any/c])
         (result/c immutable-sorted-map? any/c)]{

 Functionally inserts a mapping from @racket[key] to @racket[value] into @racket[map] if @racket[map]
 does not already contain a mapping for @racket[key]. If @racket[map] already contains @racket[key],
 a @racket[failure] containing the preexisting value is returned. Otherwise, returns a
 @racket[success] value of a new immutable sorted map containing all of the entries of @racket[map]
 and the additional inserted mapping from @racket[key] to @racket[value]. The input map is not
 modified.}


@defproc[(sorted-map-put-if-absent! [map mutable-sorted-map?] [key any/c] [value any/c])
         option?]{

 Inserts a mapping from @racket[key] to @racket[value] into @racket[map] if @racket[map]
 does not already contain a mapping for @racket[key]. If @racket[map] already contains @racket[key],
 the map is not modified and the preexisting value is returned.}


@defproc[(sorted-map-update
          [map immutable-sorted-map?]
          [key any/c]
          [updater (-> any/c any/c)]
          [failure-result failure-result/c (λ () (raise ...))])
         immutable-sorted-map?]{

 Functionally updates the value for @racket[key] in @racket[map] by applying @racket[updater] to the
 value and returning a new immutable sorted map containing all the entries of @racket[map] along with
 a mapping from @racket[key] to the value returned by @racket[updater]. If no value exists for
 @racket[key], then @racket[failure-result] determines the value passed to @racket[updater]: if it's a
 procedure it's called with no arguments to produce the value, if it's not a procedure it's passed to
 @racket[updater] directly. The input @racket[map] is not modified.}


@defproc[(sorted-map-update!
          [map mutable-sorted-map?]
          [key any/c]
          [updater (-> any/c any/c)]
          [failure-result failure-result/c (λ () (raise ...))])
         void?]{

 Updates the value for @racket[key] in @racket[map] by applying @racket[updater] to the value and
 using the returned value as the new value for @racket[key]. If no value exists for @racket[key], then
 @racket[failure-result] determines the value passed to @racket[updater]: if it's a procedure it's
 called with no arguments to produce the value, if it's not a procedure it's passed to
 @racket[updater] directly.}


@defproc[(sorted-map-remove [map immutable-sorted-map?] [key any/c]) immutable-sorted-map?]{

 Functionally removes the mapping for @racket[key] from @racket[map] by returning a new immutable
 sorted map with all of the entries in @racket[map] except for the entry for @racket[key]. The input
 @racket[map] is not modified.}


@defproc[(sorted-map-remove! [map mutable-sorted-map?] [key any/c]) void?]{

 Removes the mapping for @racket[key] from @racket[map].}


@defproc[(sorted-map-remove-all [map immutable-sorted-map?] [keys (sequence/c any/c)])
         immutable-sorted-map?]{

 Functionally removes the mappings for @racket[keys] from @racket[map] by returning a new immutable
 sorted map with all of the entries in @racket[map] except for the entries for @racket[keys]. The
 input @racket[map] is not modified. Duplicate @racket[keys] are ignored.}


@defproc[(sorted-map-remove-all! [map mutable-sorted-map?] [keys (sequence/c any/c)])
         void?]{

 Removes the mappings for @racket[keys] from @racket[map]. Duplicate @racket[keys] are ignored.}


@defproc[(sorted-map-clear! [map mutable-sorted-map?]) void?]{

 Removes all entries from @racket[map]. On its own this operation isn't all that useful, but it can be
 composed with @racket[sorted-submap] to delete a range within a sorted map.}
