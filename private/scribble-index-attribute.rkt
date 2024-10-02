#lang racket/base

(module doc racket/base

  (provide index-attribute)

  (require (for-syntax racket/base
                       racket/symbol)
           scribble/manual
           syntax/parse/define)

  ;@----------------------------------------------------------------------------

  ;; This is used to document the attributes of syntax classes. It indexes them
  ;; in Scribble as class.attribute, so they can be searched.
  (define-syntax-parse-rule (index-attribute class-id:id
                                             attribute-id:id
                                             (~and ellipsis (~literal ...)) ...)
    #:with attribute-string (symbol->immutable-string (syntax-e #'attribute-id))
    #:with (ellipses-string ...) (for/list ([_ (in-range (length (syntax->list #'(ellipsis ...))))])
                                   #`'" ...")
    #:with indexed-word (format "~a.~a" (syntax-e #'class-id) (syntax-e #'attribute-id))
    (index* (list 'indexed-word)
            (list (racketidfont 'indexed-word))
            (racketidfont 'attribute-string ellipses-string ...))))
