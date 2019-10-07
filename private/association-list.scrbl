#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/association-list
                     rebellion/collection/entry
                     rebellion/collection/immutable-vector
                     rebellion/collection/multiset)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/association-list
                   'rebellion/collection/entry
                   'rebellion/collection/multiset)
    #:private (list 'racket/base)))

@title{Association Lists}
@defmodule[rebellion/collection/association-list]

An @deftech{association list} is a collection of key-value mappings, where
multiple values for the same key are allowed. Distinct key-value mappings are
unordered, but order is preserved for mappings with the same key. The
implementation of association lists behaves similarly to a hash mapping keys to
nonempty lists, but the interface is based on a single flattened collection of
key-value pairs.

@defproc[(association-list? [v any/c]) boolean?]{
 A predicate for @tech{association lists}.}

@defproc[(association-list [k any/c] [v any/c] ... ...) association-list?]{
 Constructs an @tech{association list} containing a mapping from each @racket[k]
 to each @racket[v]. Duplicate keys are allowed, and their relative order
 determines the order of the values in the association list. The relative order
 of mappings for distinct keys is insignificant. Two association lists are equal
 if they contain the same key-value mappings and if their values for each key
 are in the same order.

 @(examples
   #:eval (make-evaluator) #:once
   (define assoc
     (association-list 'weasley 'fred
                       'weasley 'george
                       'potter 'harry
                       'granger 'hermione
                       'weasley 'ron
                       'potter 'lily))
   (association-list-ref assoc 'weasley)
   (association-list-keys assoc)
   (association-list-contains-value? assoc 'harry))}

@defproc[(association-list-size [assoc association-list?]) natural?]{
 Returns the number of key-value mappings in @racket[assoc]. Note that this does
 @bold{not} return the number of distinct keys in @racket[assoc] --- mappings
 for the same key and duplicate mappings both contribute to the returned size.

 @(examples
   #:eval (make-evaluator) #:once
   (association-list-size (association-list 'a 1 'b 2))
   (association-list-size (association-list 'a 1 'a 2 'a 3))
   (association-list-size (association-list 'a 1 'a 1 'a 1)))}

@defproc[(association-list-ref [assoc association-list?] [k any/c])
         immutable-vector?]{
 Returns the values mapped by @racket[k] in @racket[assoc]. The order of the
 mappings in @racket[assoc] is reflected in the order of the returned values,
 and duplicate mappings are preserved. If @racket[assoc] does not contain
 @racket[k], returns an empty vector.

 @(examples
   #:eval (make-evaluator) #:once
   (association-list-ref (association-list 'a 1 'b 2) 'a)
   (association-list-ref (association-list 'a 1 'b 2 'a 3) 'a)
   (association-list-ref (association-list 'a 1 'b 2 'b 2) 'b)
   (association-list-ref (association-list 'a 1 'a 1) 'a))}

@defproc[(association-list-keys [assoc association-list?]) multiset?]{
 Returns a @tech{multiset} containing the keys in @racket[assoc]. Duplicates are
 preserved, so the size of the returned multiset is equal to the size of
 @racket[assoc].

 @(examples
   #:eval (make-evaluator) #:once
   (association-list-keys (association-list 'a 1 'b 2 'c 3))
   (association-list-keys (association-list 'a 1 'a 2 'a 3))
   (association-list-keys (association-list 'a 1 'a 1 'b 2 'b 2 'c 3)))}

@defproc[(association-list-unique-keys [assoc association-list?])
         immutable-set?]{
 Returns a @tech/reference{set} containing the keys in @racket[assoc], without duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (association-list-unique-keys (association-list 'a 1 'b 2 'c 3))
   (association-list-unique-keys (association-list 'a 1 'b 2 'a 3)))}

@defproc[(association-list-values [assoc association-list?]) immutable-vector?]{
 Returns a vector containing the values in @racket[assoc] in a partially
 unspecified order. Values mapped by the same key occur next to each other in
 the returned vector and their relative order is preserved, but no order is
 preserved between values mapped by different keys. Duplicates are preserved as
 well, so the length of the returned vector is equal to the size of
 @racket[assoc].

 @(examples
   #:eval (make-evaluator) #:once
   (association-list-values (association-list 'a 1 'b 2 'c 3 'a 1 'a 3 'c 2))
   (association-list-values (association-list 'a 1 'a 1 'a 1))
   (association-list-values (association-list 'a 1 'b 2 'a 3 'b 4)))}

@defproc[(association-list-entries [assoc association-list?])
         (vectorof entry? #:immutable #t)]{
 Returns all key-value mappings in @racket[assoc], as an immutable vector of
 @tech{entries}. The same ordering guarantees as @racket[
 association-list-values] apply to the returned entries.

 @(examples
   #:eval (make-evaluator) #:once
   (association-list-entries (association-list 'a 1 'b 2 'a 3 'c 4)))}

@defproc[(association-list-contains-key? [assoc association-list?] [k any/c])
         boolean?]{
 Returns @racket[#t] if @racket[assoc] contains any mappings for the key
 @racket[k], returns @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (define assoc (association-list 'a 1 'b 2 'c 3 'a 4))
   (association-list-contains-key? assoc 'a)
   (association-list-contains-key? assoc 'banana))}

@defproc[(association-list-contains-value? [assoc association-list?] [v any/c])
         boolean?]{
 Returns @racket[#t] if @racket[assoc] contains any mappings with the value
 @racket[v], returns @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (define assoc (association-list 'a 1 'b 2 'c 3 'a 4))
   (association-list-contains-key? assoc 3)
   (association-list-contains-key? assoc 10))}

@defproc[(association-list-contains-entry? [assoc association-list?] [e entry?])
         boolean?]{
 Returns @racket[#t] if @racket[assoc] contains a mapping equal to @racket[e],
 returns @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (define assoc (association-list 'a 1 'b 2 'c 3 'a 4))
   (association-list-contains-entry? assoc (entry 'a 4))
   (association-list-contains-entry? assoc (entry 'a 2))
   (association-list-contains-entry? assoc (entry 'c 1)))}

@defproc[(association-list->hash [assoc association-list?])
         (hash/c any/c nonempty-immutable-vector?
                 #:immutable #t)]{
 Converts @racket[assoc] into a hash table mapping keys to immutable vectors of
 their values.

 @(examples
   #:eval (make-evaluator) #:once
   (association-list->hash
    (association-list 'a 1
                      'b 2
                      'c 3
                      'b 4
                      'b 5
                      'c 6
                      'a 7)))}

@defthing[empty-association-list empty-association-list?]{
 The empty @tech{association list}, which contains no key-value mappings.}

@defproc[(empty-association-list? [v any/c]) boolean?]{
 A predicate for empty @tech{association lists}. Implies @racket[
 association-list?].}

@defproc[(nonempty-association-list? [v any/c]) boolean?]{
 A predicate for nonempty @tech{association lists}. Implies @racket[
 association-list?].}
