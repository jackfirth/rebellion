#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/collection/entry
                     rebellion/collection/hash
                     rebellion/streaming/transducer
                     rebellion/type/record)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/entry
                   'rebellion/collection/hash
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
   (entry "banana" 'yellow))}

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
 the key of each entry must satisfy @racket[key-predicate].}

@defproc[(filtering-values [value-predicate predicate/c]) transducer?]{
 Like @racket[filtering], but the sequence must be made of @tech{entries} and
 the value of each entry must satisfy @racket[value-predicate].}
