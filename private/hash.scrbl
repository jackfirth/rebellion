#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/collection/entry
                     rebellion/collection/hash
                     rebellion/streaming/reducer
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

@title{Hash Tables}
@defmodule[rebellion/collection/hash]

@deftogether[[
 @defproc[(immutable-hash? [v any/c]) boolean?]
 @defproc[(mutable-hash? [v any/c]) boolean?]]]{
 Convenience predicates for immutable and mutable hash tables, respectively.
 Implies @racket[hash?].}

@deftogether[[
 @defthing[into-hash reducer?]
 @defthing[into-mutable-hash reducer?]]]{
 A pair of @tech{reducers} that build either an immutable hash table or a
 mutable hash table from a sequence of @tech{entries}.

 @(examples
   #:eval (make-evaluator) #:once
   (for/reducer into-hash
                ([str (immutable-string-split "The quick brown fox")])
     (entry (string-downcase str)
            (immutable-string-length str))))}

@defthing[empty-hash immutable-hash? #:value (hash)]{
 The empty (immutable) hash table.}

@defproc[(hash-set-entry [h immutable-hash?] [e entry?]) immutable-hash?]{
 Like @racket[hash-set], but accepting a single @tech{entry} argument instead of
 separate key and value arguments.}

@defproc[(hash-set-entry! [h mutable-hash?] [e entry?]) void?]{
 Like @racket[hash-set!], but accepting a single @tech{entry} argument instead
 of separate key and value arguments.}
