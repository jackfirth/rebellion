#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [make-named-tuple-custom-write
   (-> uninitialized-tuple-descriptor? natural?
       (-> any/c output-port? (or/c #t #f 0 1) void?))]))

(require racket/math
         rebellion/tuple-type)

;@------------------------------------------------------------------------------

(define (make-named-tuple-custom-write descriptor name-field-position)
  (define type-name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define prefix (string-append "#<" (symbol->string type-name)))
  (λ (this out _)
    (write-string prefix out)
    (define name (accessor this name-field-position))
    (when name
      (write-string ":" out)
      (write-string (symbol->string name) out))
    (write-string ">" out)))
