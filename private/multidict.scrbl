#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/entry
                     rebellion/collection/multidict
                     rebellion/collection/multiset
                     rebellion/streaming/reducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/entry
                   'rebellion/collection/multidict
                   'rebellion/collection/multiset
                   'rebellion/streaming/reducer)
    #:private (list 'racket/base)))

@title{Multidicts}
@defmodule[rebellion/collection/multidict]

A @deftech{multidict} is an unordered collection of key-value mappings, where
multiple unique values for the same key are allowed. The implementation of
multidicts behaves similarly to a hash from keys to nonempty sets, but the
interface is based on a flattened collection of key-value pairs.

@defproc[(multidict? [v any/c]) boolean?]{
 A predicate for @tech{multidicts}.}

@defproc[(multidict [k any/c] [v any/c] ... ...) multidict?]{
 Constructs a @tech{multidict} containing a mapping from each @racket[k] to each
 @racket[v]. Multiple values are allowed for the same key, but duplicate values
 for a key are removed. The order of key-value pairs is insignificant. Two
 multidicts are equal if they contain the same mappings.
                                                             
 @(examples
   #:eval (make-evaluator) #:once
   (multidict 'a 1 'b 2 'c 3)
   (multidict 'a 1 'a 2 'a 3)
   (multidict 'a 1 'a 1 'b 1))}

@defproc[(multidict-ref [dict multidict?] [k any/c]) immutable-set?]{
 Returns the set of values mapped by @racket[k] in @racket[dict].
                                                                     
 @(examples
   #:eval (make-evaluator) #:once
   (define dict
     (multidict 'fruit 'apple
                'fruit 'orange
                'fruit 'banana
                'vegetable 'carrot
                'vegetable 'celery))
   (multidict-ref dict 'fruit)
   (multidict-ref dict 'vegetable)
   (multidict-ref dict 'dessert))}

@defproc[(multidict-add [dict multidict?] [k any/c] [v any/c]) multidict?]{
 Adds a mapping from @racket[k] to @racket[v] in @racket[dict], returning a new
 updated @tech{multidict}. If @racket[dict] already contains an entry for
 @racket[k] and @racket[v], the dict is returned unchanged.

 @(examples
   #:eval (make-evaluator) #:once
   (multidict-add (multidict 'a 1) 'b 2)
   (multidict-add (multidict 'a 1) 'a 2)
   (multidict-add (multidict 'a 1) 'a 1))}

@defproc[(multidict-add-entry [dict multidict?] [e entry?]) multidict?]{
 Like @racket[multidict-add], but with the key and value wrapped in the @tech{
  entry} @racket[e].

 @(examples
   #:eval (make-evaluator) #:once
   (multidict-add-entry (multidict 'a 1) (entry 'b 2))
   (multidict-add-entry (multidict 'a 1) (entry 'a 2))
   (multidict-add-entry (multidict 'a 1) (entry 'a 1)))}

@defproc[(multidict-size [dict multidict?]) natural?]{
 Returns the number of key-value mappings in @racket[dict]. Note that this does
 @bold{not} return the number of keys in @racket[dict] --- all values mapped by
 a key contribute to the returned size.
                                                      
 @(examples
   #:eval (make-evaluator) #:once
   (multidict-size (multidict 'a 1 'b 2 'c 3))
   (multidict-size (multidict 'a 1 'b 2 'b 3))
   (multidict-size (multidict 'a 1 'a 1 'a 1)))}

@defproc[(multidict-keys [dict multidict?]) multiset?]{
 Returns a @tech{multiset} of the keys in @racket[dict], with a copy of each
 key for each value mapped by that key.
                                                       
 @(examples
   #:eval (make-evaluator) #:once
   (multidict-keys
    (multidict 'fruit 'apple
               'fruit 'orange
               'fruit 'banana
               'vegetable 'carrot
               'vegetable 'celery)))}

@defproc[(multidict-values [dict multidict?]) multiset?]{
 Returns a @tech{multiset} of all values in @racket[dict].
                                                         
 @(examples
   #:eval (make-evaluator) #:once
   (multidict-values
    (multidict "Iron Man" 'marvel
               "Superman" 'dc
               "The Black Panther" 'marvel
               "Wonder Woman" 'dc
               "The Hulk" 'marvel
               "Captain Marvel" 'marvel)))}

@defproc[(multidict-unique-keys [dict multidict?]) immutable-set?]{
 Returns the set of keys in @racket[dict], ignoring duplicates.
                                                                   
 @(examples
   #:eval (make-evaluator) #:once
   (multidict-unique-keys
    (multidict 'fruit 'apple
               'fruit 'orange
               'fruit 'banana
               'vegetable 'carrot
               'vegetable 'celery)))}

@defproc[(multidict-entries [dict multidict?]) (immutable-set/c entry?)]{
 Returns the set of @tech{entries} in @racket[dict]. Note that this is
 @bold{not} a @tech{multiset}, because for each key the collection of values
 mapped by that key contains no duplicates.

 @(examples
   #:eval (make-evaluator) #:once
   (multidict-entries
    (multidict 'fruit 'apple
               'fruit 'orange
               'fruit 'banana
               'vegetable 'carrot
               'vegetable 'celery)))}

@defproc[(multidict->hash [dict multidict?])
         (hash/c any/c nonempty-immutable-set? #:immutable? #t)]{
 Converts @racket[dict] into a hash table from keys to (nonempty) sets of
 values.

 @(examples
   #:eval (make-evaluator) #:once
   (multidict->hash
    (multidict 'fruit 'apple
               'fruit 'orange
               'fruit 'banana
               'vegetable 'carrot
               'vegetable 'celery)))}

@defproc[(multidict-inverse [dict multidict?]) multidict?]{
 Inverts @racket[dict], returning a multidict with all the same entries as
 @racket[dict] except keys and values are swapped.

 @(examples
   #:eval (make-evaluator) #:once
   (multidict-inverse
    (multidict 'a 1
               'a 2
               'a 3
               'b 2
               'b 4
               'c 4
               'c 1
               'd 3)))}

@defproc[(multidict-contains-key? [dict multidict?] [k any/c]) boolean?]{
 Returns @racket[#t] if @racket[dict] contains any mappings for the key
 @racket[k], returns @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (define dict (multidict 'even 2 'even 4 'odd 1 'odd 5 'odd 3))
   (multidict-contains-key? dict 'even)
   (multidict-contains-key? dict 'prime))}

@defproc[(multidict-contains-value? [dict multidict?] [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[dict] contains any mappings with the value
 @racket[v], returns @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (define dict (multidict 'even 2 'even 4 'odd 1 'odd 5 'odd 3))
   (multidict-contains-value? dict 5)
   (multidict-contains-value? dict "hello world"))}

@defproc[(multidict-contains-entry? [dict multidict?] [e entry?]) boolean?]{
 Returns @racket[#t] if @racket[dict] contains the mapping @racket[e], returns
 @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (define dict (multidict 'even 2 'even 4 'odd 1 'odd 5 'odd 3))
   (multidict-contains-entry? dict (entry 'even 4))
   (multidict-contains-entry? dict (entry 'odd 4)))}

@defthing[empty-multidict empty-multidict?]{
 The empty @tech{multidict}, which contains nothing.}

@defproc[(empty-multidict? [v any/c]) boolean?]{
 A predicate for empty @tech{multidicts}. Implies @racket[multidict?].}

@defproc[(nonempty-multidict? [v any/c]) boolean?]{
 A predicate for nonempty @tech{multidicts}. Implies @racket[multidict?].}

@section{Multidict Iterations and Comprehensions}

@defproc[(in-multidict-entries [dict multidict?]) (sequence/c entry?)]{
 Returns a @tech{sequence} of the @tech{entries} in @racket[dict].

 @(examples
   #:eval (make-evaluator) #:once
   (define food-classifications
     (multidict 'fruit 'apple
                'vegetable 'carrot
                'fruit 'orange
                'fruit 'banana
                'vegetable 'celery))
   (for ([e (in-multidict-entries food-classifications)])
     (printf "key: ~a    value: ~a"
             (entry-key e)
             (entry-value e))))}

@defform[(for/multidict (for-clause ...) body-or-break ... body)
         #:contracts ([body entry?])]{
 Iterates like @racket[for], but collects each @tech{entry} returned by @racket[
 body] into a @tech{multidict}.}

@defform[(for*/multidict (for-clause ...) body-or-break ... body)
         #:contracts ([body entry?])]{
 Iterates like @racket[for*], but collects each @tech{entry} returned by
 @racket[body] into a @tech{multidict}.}

@defthing[into-multidict reducer?]{
 A @tech{reducer} that reduces a sequence of @tech{entries} into a
 @tech{multidict}.}
