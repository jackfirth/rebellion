#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/record)
          scribble/example)

@(define module-sharing-evaluator-factory
   (make-base-eval-factory (list 'racket/base 'rebellion/record)))

@(define (make-evaluator)
   (define evaluator (module-sharing-evaluator-factory))
   (evaluator '(require rebellion/record))
   evaluator)

@title{Records}
@defmodule[rebellion/record]

A @deftech{record} maps each of its keywords to a single value. Records are
similar to hash tables, except keys @emph{must} be keywords. Records are less
dynamic than general-purpose hash tables, but their specialized nature can offer
improved performance. In particular, constructing a record with @racket[record]
and calling a keyword-accepting function with a record impose only constant-time
overhead. Use records instead of hash tables when keys are expected to be
literal names written in source code. As a rule of thumb, if you find yourself
reaching for a hash table whose keys are symbols or strings, use records
instead.

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

@defproc[(record-keywords [rec record?]) (listof keyword?)]{
 Returns the keywords contained in @racket[rec], sorted in ascending order by
 @racket[keyword<?].

 @(examples
   #:eval (make-evaluator) #:once
   (define rec
     (record #:name "Alyssa P. Hacker"
             #:age 42
             #:favorite-color 'turqoise))
   (record-keywords rec))}

@defproc[(record-values [rec record?]) list?]{
 Returns the values contained in @racket[rec], in the same order as the value's
 corresponding keyword in @racket[(record-keywords rec)].

 @(examples
   #:eval (make-evaluator) #:once
   (define rec
     (record #:name "Alyssa P. Hacker"
             #:age 42
             #:favorite-color 'turqoise))
   (record-values rec))}

@defproc[(record-size [rec record?]) natural?]{
 Returns the number of keyword-value entries in @racket[rec].

 @(examples
   #:eval (make-evaluator) #:once
   (define rec
     (record #:name "Alyssa P. Hacker"
             #:age 42
             #:favorite-color 'turqoise))
   (record-size rec))}
