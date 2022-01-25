#lang racket/base


(provide for-body)


(require syntax/for-body
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-splicing-syntax-class (for-body context)
  #:attributes ([pre-body 1] [post-body 1])
  (pattern (~seq body ... tail-expr:expr)
    #:with ((pre-body ...) (post-body ...)) (split-for-body context #'(body ... tail-expr))))
