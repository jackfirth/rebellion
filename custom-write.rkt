#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [custom-write-mode/c flat-contract?]
  [custom-write-function/c chaperone-contract?]
  [object-name/c flat-contract?]
  [make-named-object-custom-write
   (->* (symbol?) (#:name-getter (-> any/c (or/c object-name/c #false)))
        custom-write-function/c)]))

;@------------------------------------------------------------------------------

(define custom-write-mode/c (or/c boolean? 0 1))

(define custom-write-function/c
  (-> any/c output-port? custom-write-mode/c void?))

(define object-name/c (or/c symbol? string? bytes? number? path?))

(define (object-name->string name)
  (cond
    [(symbol? name) (symbol->string name)]
    [(string? name) name]
    [(bytes? name) (bytes->string/locale name #\�)]
    [(number? name) (number->string name)]
    [(path? name) (path->string name)]))

(define (make-named-object-custom-write type-name
                                        #:name-getter [get-name object-name])
  (define type-part (string-append "#<" (symbol->string type-name)))
  (λ (this out mode)
    (parameterize ([current-output-port out])
      (write-string type-part)
      (define name (get-name this))
      (when name
        (write-string ":")
        (write-string (object-name->string name)))
      (write-string ">"))
    (void)))
