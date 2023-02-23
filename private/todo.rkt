#lang racket/base


(provide TODO)


(require (for-syntax racket/base
                     syntax/parse))


;@----------------------------------------------------------------------------------------------------


(define-syntax (TODO stx)
  (syntax-parse stx
    #:track-literals
    #:literals (TODO)
    [TODO
     #:with original stx
     #'(raise-syntax-error #false "not yet implemented" #'original)]))
