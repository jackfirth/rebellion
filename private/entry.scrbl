#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/match
                     racket/sequence
                     racket/set
                     rebellion/collection/entry
                     rebellion/collection/hash
                     rebellion/collection/multidict
                     rebellion/collection/set
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer
                     rebellion/type/record)
          (submod rebellion/private/scribble-cross-document-tech doc)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'racket/match
                   'racket/set
                   'rebellion/collection/entry
                   'rebellion/collection/hash
                   'rebellion/collection/multidict
                   'rebellion/collection/set
                   'rebellion/streaming/reducer
                   'rebellion/streaming/transducer
                   'rebellion/type/record)
    #:private (list 'racket/base)))

@title{Entries}
@defmodule[rebellion/collection/entry]

An @deftech{entry} is a key-value pair representing a mapping in a
dictionary-like collection. Entries are semantically equivalent to @tech{pairs},
but use the less generic and more readable names @racket[entry-key] and @racket[
 entry-value]. Use entries instead of pairs when working with dicitonary-like
types, as a collection of @racket[entry?] values has a clearer intended purpose
than a collection of @racket[pair?] values.

@defproc[(entry? [v any/c]) boolean?]{
 A predicate for @tech{entries}.}

@defproc[(entry [k any/c] [v any/c]) entry?]{
 Constructs an @tech{entry}.

 @(examples
   #:eval (make-evaluator) #:once
   (entry "apple" 'red)
   (entry "banana" 'yellow))

 Additionally, @racket[entry] can be used as a @tech/reference{match expander}
 with @racket[match].

 @(examples
   #:eval (make-evaluator) #:once
   (match (entry "apple" 'red)
     [(entry k v) (entry v k)]))}

@defproc[(entry-key [e entry?]) any/c]{
 Returns the key of @racket[e].

 @(examples
   #:eval (make-evaluator) #:once
   (entry-key (entry "apple" 'red)))}

@defproc[(entry-value [e entry?]) any/c]{
 Returns the value in @racket[e].

 @(examples
   #:eval (make-evaluator) #:once
   (entry-value (entry "banana" 'yellow)))}

@defproc[(bisecting [key-function (-> any/c any/c)]
                    [value-function (-> any/c any/c)])
         transducer?]{
 Constructs a @tech{transducer} that transforms each element of a sequence into
 an @tech{entry}, by applying @racket[key-function] and @racket[value-function]
 to the element to extract the key and value of the entry.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-record-type book (title author))
    (define library
      (list (book #:title "War and Peace" #:author "Leo Tolstoy")
            (book #:title "To the Lighthouse" #:author "Virginia Woolf")
            (book #:title "Frankenstein" #:author "Mary Shelley"))))

   (transduce library
              (bisecting book-author book-title)
              #:into into-hash))}

@defproc[(indexing [key-function (-> any/c any/c)]) transducer?]{
 Constructs a @tech{transducer} that transforms a sequence of elements into a
 sequence of @tech{entries} keyed by @racket[key-function].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-record-type book (title author))
    (define library
      (list (book #:title "War and Peace" #:author "Leo Tolstoy")
            (book #:title "To the Lighthouse" #:author "Virginia Woolf")
            (book #:title "Frankenstein" #:author "Mary Shelley"))))

   (transduce library
              (indexing book-title)
              #:into into-hash))}

@defproc[(mapping-keys [key-function (-> any/c any/c)]) transducer?]{
 Like @racket[mapping], but the sequence must be made of @tech{entries} and
 @racket[key-function] is applied to the key of each entry.}

@defproc[(mapping-values [value-function (-> any/c any/c)]) transducer?]{
 Like @racket[mapping], but the sequence must be made of @tech{entries} and
 @racket[value-function] is applied to the key of each entry.}

@defproc[(filtering-keys [key-predicate predicate/c]) transducer?]{
 Like @racket[filtering], but the sequence must be made of @tech{entries} and
 the key of each entry must satisfy @racket[key-predicate].

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-hash-entries (hash "the" 0 "quick" 1 'brown 2 'fox 3))
              (filtering-keys symbol?)
              #:into into-hash))}

@defproc[(filtering-values [value-predicate predicate/c]) transducer?]{
 Like @racket[filtering], but the sequence must be made of @tech{entries} and
 the value of each entry must satisfy @racket[value-predicate].

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-hash-entries (hash 'a 1 'b 2 'c 3 'd 4))
              (filtering-values even?)
              #:into into-hash))}

@defproc[(append-mapping-keys
          [key-sequence-function (-> any/c (sequence/c any/c))])
         transducer?]{
 Constructs a @tech{transducer} that transforms a sequence of @tech{entries} by
 applying @racket[key-sequence-function] to the key of each entry, then emitting
 one entry per key in the sequence returned by @racket[key-sequence-function].

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-hash-entries (hash "hello" 'hello "world" 'world))
              (append-mapping-keys in-string)
              (deduplicating #:key entry-key)
              #:into into-hash))}

@defproc[(append-mapping-values
          [value-sequence-function (-> any/c (sequence/c any/c))])
         transducer?]{
 Constructs a @tech{transducer} that transforms a sequence of @tech{entries} by
 applying @racket[value-sequence-function] to the value of each entry, then
 emitting one entry per value in the sequence returned by @racket[
 value-sequence-function].

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (in-hash-entries (hash 'apple (set 1 2 3) 'banana (set 4 5 6)))
              (append-mapping-values in-immutable-set)
              #:into into-set))}

@defthing[batching-into-entries transducer?]{
 A @tech{transducer} that transforms a flat sequences of alternating keys and
 values into a sequence of @tech{entries}. The sequence must contain an even
 number of elements, otherwise a contract failure is raised.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (list 'a 1 'b 2 'c 3)
              batching-into-entries
              #:into into-set)
   (eval:error
    (transduce (list 'a 1 'b 2 'c)
               batching-into-entries
               #:into into-set)))}

@defproc[(grouping [value-reducer reducer?]) transducer?]{
 Constructs a @tech{transducer} that transforms a sequence of @tech{entries} by
 merging entries with equal keys. For each key, the values of all entries with
 that key are combined using @racket[value-reducer]. If @racket[value-reducer]
 finishes early, its result is emitted and any remaining values for that key are
 discarded.

 @(examples
   #:eval (make-evaluator) #:once
   (transduce (multidict 'a 1 'b 2 'a 3 'b 4 'a 5)
              (grouping into-sum)
              #:into into-hash))}
