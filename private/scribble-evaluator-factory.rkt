#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [evaluator/c contract?]
  [evaluator-factory? (-> any/c boolean?)]
  [evaluator-factory-create (-> evaluator-factory? evaluator/c)]
  [make-module-sharing-evaluator-factory
   (->* ()
        (#:private (listof module-path?)
         #:public (listof module-path?))
        evaluator-factory?)]))

(require racket/list
         scribble/example)

;@------------------------------------------------------------------------------

(define evaluator/c (-> any/c any/c))

(struct evaluator-factory (thunk)
  #:constructor-name make-evaluator-factory
  #:property prop:procedure 0)

(define (evaluator-factory-create factory)
  ((evaluator-factory-thunk factory)))

(define (make-module-sharing-evaluator-factory
         #:public [public-modules empty]
         #:private [private-modules empty])
  (define base-factory
    (make-base-eval-factory (append private-modules public-modules)))
  (define (factory)
    (define evaluator (base-factory))
    (evaluator `(require ,@public-modules))
    evaluator)
  (make-evaluator-factory factory))
