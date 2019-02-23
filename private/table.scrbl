#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     rebellion/table)
          rebellion/private/scribble-evaluator-factory
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/table)
    #:private (list 'racket/base)))

@title{Tables}
@defmodule[rebellion/table]

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

@defproc[(table-columns-ref [tab table?] [column keyword?]) list?]{
 Returns a list of all the values for @racket[column] in @racket[tab], with each
 element of the list corresponding to one row in @racket[tab].

 @(examples
   #:eval (make-evaluator) #:once
   (define countries
     (table (columns #:name #:population #:capital-city)
            (row "Argentina" 43800000 "Buenos Aires")
            (row "Greece" 10800000 "Athens")
            (row "Nigeria" 198600000 "Abuja")
            (row "Japan" 126400000 "Tokyo")))
   (table-columns-ref countries '#:capital-city))}
