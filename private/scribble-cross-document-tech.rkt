#lang racket/base

(module doc racket/base

  (require racket/contract/base)

  (provide
   (contract-out
    ;; Similar to @tech{....}, but creates a link to a `deftech` in The Racket
    ;; Guide.
    [tech/guide cross-document-tech-function/c]
    ;; Links to a `deftech` in The Racket Reference.
    [tech/reference cross-document-tech-function/c]
    ;; Links to a `deftech` in the `syntax/` libraries.
    [syntax-tech cross-document-tech-function/c]))

  (require rebellion/base/immutable-string
           scribble/base
           scribble/core
           scribble/decode
           scribble/manual)

  ;@----------------------------------------------------------------------------

  (define cross-document-tech-function/c
    (->* ()
         (#:key (or/c string? #f) #:normalize? boolean?)
         #:rest (listof pre-content?)
         element?))

  (define (tech/guide #:key [key #f] #:normalize? [normalize? #t] . text)
    (apply tech
           #:doc '(lib "scribblings/guide/guide.scrbl")
           #:key key
           #:normalize? normalize?
           text))

  (define (tech/reference #:key [key #f] #:normalize? [normalize? #t] . text)
    (apply tech
           #:doc '(lib "scribblings/reference/reference.scrbl")
           #:key key
           #:normalize? normalize?
           text))

  (define (syntax-tech #:key [key #f] #:normalize? [normalize? #t] . text)
    (apply tech
           #:doc '(lib "syntax/scribblings/syntax.scrbl")
           #:key key
           #:normalize? normalize?
           text)))
