#lang scribble/manual

@(require (for-label net/url
                     racket/base
                     racket/contract/base
                     rebellion/web-link)
          rebellion/private/scribble-evaluator-factory
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/web-link 'net/url)
    #:private (list 'racket/base)))

@title{Web Links}
@defmodule[rebellion/web-link]

@(define rfc8288 "https://tools.ietf.org/html/rfc8288")

A @deftech{web link} is a relationship between two resources, represented as a
source-relationship-target triple. See @hyperlink[rfc8288]{RFC 8288 - Web
 Linking} for more information on the purpose and uses of links.

@defproc[(web-link? [v any/c]) boolean?]{
 A predicate for @tech{web links}.}

@defproc[(web-link [source url?] [relation (or/c symbol? url?)] [target url?])
         web-link?]{
 Constructs a @tech{web link} from @racket[source] to @racket[target] with type
 @racket[relation].

 @(examples
   #:eval (make-evaluator) #:once
   (web-link (string->url "http://example.org")
             'stylesheet
             (string->url "http://example.org/css")))}

@deftogether[[
 @defproc[(web-link-source [link web-link?]) url?]
 @defproc[(web-link-relation [link web-link?]) (or/c symbol? url?)]
 @defproc[(web-link-target [link web-link?]) url?]]]{
 Accessors for the various components of a @tech{web link}.}
