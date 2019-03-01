#lang racket/base

(provide
 define-struct-field-accessors)

(require (for-syntax racket/base
                     racket/syntax)
         rebellion/struct-descriptor
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-struct-field-accessors struct:id (field:id ...)
    #:descriptor descriptor:expr)
  #:with (field-accessor:id ...)
  (map (λ (field-id)
         (format-id field-id "~a-~a" (syntax-e #'struct) (syntax-e field-id)
                    #:source field-id #:props field-id))
       (syntax->list #'(field ...)))
  #:with (position:nat ...)
  (build-list (length (syntax->list #'(field ...))) values)
  (begin
    (define accessor (struct-descriptor-accessor descriptor))
    (define field-accessor
      (make-struct-field-accessor accessor 'position 'field))
    ...))
