#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [generative-token? (-> any/c boolean?)]
  [make-generative-token (-> generative-token?)]))

;@------------------------------------------------------------------------------

(struct generative-token () #:constructor-name make-generative-token)
