#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/base/symbol
                     rebellion/collection/record
                     rebellion/collection/table
                     rebellion/streaming/reducer)
          (submod rebellion/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/record
                   'rebellion/collection/table
                   'rebellion/streaming/reducer)
    #:private (list 'racket/base)))

@title{Tables}
@defmodule[rebellion/collection/table]

A @deftech{table} is a data structure made up of a collection of rows. Tables
contain a list of column names --- represented by keywords --- and each row has
a value for each column in the row's table. This makes rows similar to @tech{
 records}, with the constraint that all the row records in a table have the same
keys. Tables maintain rows as lists, not sets, so the order of rows in a table
is significant and duplicate rows are allowed.

@defproc[(table? [v any/c]) boolean?]{
 A predicate for @tech{tables}.}

@defform[#:literals (columns row)
         (table (columns column-kw ...) (row row-value ...) ...)]{
 Constructs a @tech{table} containing all of the given rows and whose columns
 are named by the given @racket[column-kw]s. Row values are given by-position
 and there must be exactly one value for each @racket[column-kw].

 @(examples
   #:eval (make-evaluator) #:once
   (table (columns #:name #:population #:capital-city)
          (row "Argentina" 43800000 "Buenos Aires")
          (row "Greece" 10800000 "Athens")
          (row "Nigeria" 198600000 "Abuja")
          (row "Japan" 126400000 "Tokyo")))}

@deftogether[
 (@defform[#:id columns columns]
   @defform[#:id row row])]{
 Syntactic forms recognized by @racket[table]. Both @racket[columns] and
 @racket[row] are meaningless on their own; they can only be used in a @racket[
 table] expression.}

@defproc[(table-ref [tab table?] [pos natural?] [column keyword?]) any/c]{
 Returns the value for @racket[column] in the row at position @racket[pos] in
 @racket[tab].

 @(examples
   #:eval (make-evaluator) #:once
   (define countries
     (table (columns #:name #:population #:capital-city)
            (row "Argentina" 43800000 "Buenos Aires")
            (row "Greece" 10800000 "Athens")
            (row "Nigeria" 198600000 "Abuja")
            (row "Japan" 126400000 "Tokyo")))
   (table-ref countries 3 '#:name))}

@defproc[(table-rows-ref [tab table?] [pos natural?]) record?]{
 Returns the row in @racket[tab] at position @racket[pos], as a record mapping
 column names to their values in the row.

 @(examples
   #:eval (make-evaluator) #:once
   (define countries
     (table (columns #:name #:population #:capital-city)
            (row "Argentina" 43800000 "Buenos Aires")
            (row "Greece" 10800000 "Athens")
            (row "Nigeria" 198600000 "Abuja")
            (row "Japan" 126400000 "Tokyo")))
   (table-rows-ref countries 2))}

@defproc[(table-columns-ref [tab table?] [column keyword?]) immutable-vector?]{
 Returns a vector of all the values for @racket[column] in @racket[tab], with
 each element corresponding to one row in @racket[tab].

 @(examples
   #:eval (make-evaluator) #:once
   (define countries
     (table (columns #:name #:population #:capital-city)
            (row "Argentina" 43800000 "Buenos Aires")
            (row "Greece" 10800000 "Athens")
            (row "Nigeria" 198600000 "Abuja")
            (row "Japan" 126400000 "Tokyo")))
   (table-columns-ref countries '#:capital-city))}

@section{Table Comprehensions}

@defform[(for/table (for-clause ...) body-or-break ... body)
         #:contracts ([body record?])]{
 Iterates like @racket[for], but each @racket[body] must evaluate to a @tech{
  record} and the resulting records are collected into a @tech{table}. All
 @racket[body] records must have the same keys. If @racket[body] is never
 evaluated, for instance because the loop iterates over an empty collection,
 then an empty table with no rows and no columns is returned.

 @(examples
   #:eval (make-evaluator) #:once
   (for/table ([god (in-list (list "Zeus" "hera" "hades" "Athena" "PosEIdon"))])
     (define name (string-titlecase god))
     (record #:name name
             #:correct-case? (equal? name god)
             #:name-length (string-length god))))}

@defform[(for*/table (for-clause ...) body-or-break ... body)
         #:contracts ([body record?])]{
 Like @racket[for/table], but iterates like @racket[for*].}

@defthing[into-table reducer?]{
 A @tech{reducer} that reduces @tech{records} into a @tech{table}, in the same
 manner as @racket[for/table].

 @(examples
   #:eval (make-evaluator) #:once
   (reduce into-table
           (record #:person "Sam"
                   #:age 78
                   #:favorite-color 'green)
           (record #:person "Jamie"
                   #:age 30
                   #:favorite-color 'purple)
           (record #:person "Ned"
                   #:age 40
                   #:favorite-color 'red)))}
