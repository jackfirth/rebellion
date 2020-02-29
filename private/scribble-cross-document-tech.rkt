#lang racket/base

(module doc racket/base

  (require racket/contract/base)

  (provide
    (contract-out
      [tech/guide
        ;; Similar to @tech{....}, but creates a link to a `deftech` in The Racket Guide
        (->* () () #:rest (listof pre-content?) element?)]
      [tech/reference
        ;; Links to a `deftech` in The Racket Reference
        (->* () () #:rest (listof pre-content?) element?)]))

  (require rebellion/base/immutable-string
           scribble/base
           scribble/core
           scribble/decode
           scribble/manual)

  (define (tech/guide . text)
    (apply tech #:doc '(lib "scribblings/guide/guide.scrbl") text))

  (define (tech/reference . text)
    (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") text)))
