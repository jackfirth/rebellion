#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/collection/keyset
                     rebellion/collection/record)
          (submod rebellion/private/scribble-evaluator-factory doc)
          (submod rebellion/private/scribble-cross-document-tech doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/record 'rebellion/collection/keyset)
    #:private (list 'racket/base)))

@title{Records}
@defmodule[rebellion/collection/record]

A @deftech{record} is a collection of name-value mappings, each which is called
a @deftech{record field}. The name of a field is a @tech/reference{keyword}. Records
support constant-time lookup of field values by name.

Records are similar to hash tables, except keys @emph{must} be keywords. Records
are less dynamic than general-purpose hash tables, but their specialized nature
can offer improved performance. In particular, constructing a record with
@racket[record] does not require sorting keywords at runtime, and calling a
keyword-accepting function with a record imposes only constant-time overhead.
Use records instead of hash tables when keys are expected to be literal names
written in source code. As a rule of thumb, if you find yourself reaching for a
hash table whose keys are symbols or strings, use records instead.

@defproc[(record? [v any/c]) boolean?]{
 A predicate for @tech{records}.}

@defproc[(record [#:<kw> v any/c] ...) record?]{
 Constructs a record containing each @racket[v], where @racket[#:<kw>] stands
 for any keyword.

 @(examples
   #:eval (make-evaluator) #:once
   (record #:name "Alyssa P. Hacker"
           #:age 42
           #:favorite-color 'turqoise))}

@defproc[(record-keywords [rec record?]) keyset?]{
 Returns a @tech{keyset} of the keywords contained in @racket[rec].

 @(examples
   #:eval (make-evaluator) #:once
   (define rec
     (record #:name "Alyssa P. Hacker"
             #:age 42
             #:favorite-color 'turqoise))
   (record-keywords rec))}

@defproc[(record-values [rec record?]) immutable-vector?]{
 Returns the values contained in @racket[rec], in the same order as the value's
 corresponding keyword in @racket[(record-keywords rec)].

 @(examples
   #:eval (make-evaluator) #:once
   (define rec
     (record #:name "Alyssa P. Hacker"
             #:age 42
             #:favorite-color 'turqoise))
   (record-values rec))}

@defthing[empty-record record?]{
 The empty record, which contains no entries.}

@defproc[(record-size [rec record?]) natural?]{
 Returns the number of keyword-value entries in @racket[rec].

 @(examples
   #:eval (make-evaluator) #:once
   (define rec
     (record #:name "Alyssa P. Hacker"
             #:age 42
             #:favorite-color 'turqoise))
   (record-size rec))}

@defproc[(record-ref [rec record?] [kw keyword?]) any/c]{
 Returns the value in @racket[rec] for @racket[kw], or @racket[#f] if none
 exists.

 @(examples
   #:eval (make-evaluator) #:once
   (define rec
     (record #:name "Alyssa P. Hacker"
             #:age 42
             #:favorite-color 'turqoise))
   (record-ref rec '#:name)
   (record-ref rec '#:fur-color)))}

@defproc[(record-remove [rec record?] [kw keyword?]) record?]{
 Returns @racket[rec] with the entry for @racket[kw] removed.

 @(examples
   #:eval (make-evaluator) #:once
   (record-remove (record #:x 42 #:y 7) '#:x))}

@defproc[(build-record [builder (-> keyword? any/c)] [keys keyset?]) record?]{
 Constructs a record by calling @racket[builder] with each keyword in @racket[
 keys] in an unspecified order.

 @(examples
   #:eval (make-evaluator) #:once
   (build-record keyword->string (keyset #:x #:y #:z)))}

@defproc[(record-merge2
          [rec1 record?]
          [rec2 record?]
          [#:merge merge (-> any/c any/c any/c) (λ (a b) b)])
         record?]{
 Combines @racket[rec1] and @racket[rec2] into a single record containing the
 entries of both. If a keyword is contained in both records the values for that
 key are combined with @racket[merge]. The default merge function ignores the
 first value, causing entries in @racket[rec2] to overwrite entries in @racket[
 rec1].

 @(examples
   #:eval (make-evaluator) #:once
   (record-merge2 (record #:x 1 #:y 2)
                  (record #:name "Alyssa P. Hacker" #:age 42))
   (record-merge2 (record #:x 1 #:y 2 #:z 3)
                  (record #:z 100))
   (record-merge2 (record #:x 1 #:y 2 #:z 3)
                  (record #:x -1 #:y -2 #:z -3)
                  #:merge +))}

@defproc[(record-map [rec record?] [f (-> any/c any/c)]) record?]{
 Applies @racket[f] to each value in @racket[rec] and returns a new record
 containing the results with the same keys as @racket[rec].

 @(examples
   #:eval (make-evaluator) #:once
   (record-map (record #:x 1 #:y 2 #:z 3)
               (λ (x) (* x 100))))}

@defproc[(record-contains-key? [rec record?] [kw keyword?]) boolean?]{
 Returns @racket[#t] if @racket[rec] contains a mapping for @racket[kw], returns
 @racket[#f] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (record-contains-key? (record #:x 0 #:y 0) '#:x)
   (record-contains-key? (record #:x 0 #:y 0) '#:theta))}

@section{Record Fields}

@defproc[(record-field? [v any/c]) boolean?]{
 A predicate for @tech{record fields}.}

@defproc[(record-field [#:<kw> v any/c]) record-field?]{
 Constructs a @tech{record field} mapping whatever keyword is given for
 @racket[#:<kw>] to @racket[v].

 @(examples
   #:eval (make-evaluator) #:once
   (record-field #:title "Fabulous Widget 2.0")
   (record-field #:color 'firetruck-red))}

@deftogether[[
 @defproc[(record-field-name [field record-field?]) keyword?]
 @defproc[(record-field-value [field record-field?]) any/c]]]{
 Accessors for the name and value of a @tech{record field}.

 @(examples
   #:eval (make-evaluator) #:once
   (define field (record-field #:title "Fabulous Widget 2.0"))
   (record-field-name field)
   (record-field-value field))}
