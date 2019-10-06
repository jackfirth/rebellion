#lang racket/base

(module doc racket/base

  (require racket/contract/base
           scribble/base
           scribble/core
           scribble/decode
           scribble/manual)

  (provide
    (contract-out
      [tech/guide
        ;; Similar to @tech{....}, but creates a link to a `deftech` in The Racket Guide
        (->* () (#:key (or/c string? #f)) #:rest (listof pre-content?) element?)]
      [tech/reference
        ;; Links to a `deftech` in The Racket Reference
        (->* () (#:key (or/c string? #f)) #:rest (listof pre-content?) element?)]))

  (define (tech/guide #:key [key #f] . text)
    (keyword-apply tech '(#:doc #:key) `((lib "scribblings/guide/guide.scrbl") ,key) text))

  (define (tech/reference #:key [key #f] . text)
    (keyword-apply tech '(#:doc #:key) `((lib "scribblings/reference/reference.scrbl") ,key) text)))
