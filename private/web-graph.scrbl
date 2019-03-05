#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     rebellion/web-graph
                     rebellion/web-link)
          rebellion/private/scribble-evaluator-factory
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/web-graph 'rebellion/web-link)
    #:private (list 'racket/base)))

@title{Web Graphs}
@defmodule[rebellion/web-graph]

A @deftech{web graph} is an immutable, directed multigraph where nodes are URIs
and edges are link relations.

@defproc[(web-graph? [v any/c]) boolean?]{
 A predicate for @tech{web graphs}.}

@defproc[(web-graph [link web-link?] ...) web-graph?]{
 Constructs a @tech{web graph} containing the given @racket[link]s.

 @(examples
   #:eval (make-evaluator) #:once
   (web-graph
    (web-link "http://example.org" 'stylesheet "/styles.css")
    (web-link "http://example.org" 'stylesheet "/fonts.css")
    (web-link "http://example.org" 'search "/opensearch.xml")
    (web-link "http://example.org" 'privacy-policy "/privacy-policy")))}

@defthing[empty-web-graph web-graph?]{
 The empty @tech{web graph}.}
