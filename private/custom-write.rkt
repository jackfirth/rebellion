#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [custom-write-mode/c flat-contract?]
  [custom-write-function/c chaperone-contract?]
  [make-named-object-custom-write
   (->* (symbol?) (#:name-getter (-> any/c (or/c symbol? #f)))
        custom-write-function/c)]
  [make-constant-custom-write (-> symbol? custom-write-function/c)]))

;@------------------------------------------------------------------------------

(define custom-write-mode/c (or/c boolean? 0 1))

(define custom-write-function/c
  (-> any/c output-port? custom-write-mode/c void?))

(define (make-named-object-custom-write type-name
                                        #:name-getter [get-name object-name])
  (define type-part (string-append "#<" (symbol->string type-name)))
  (λ (this out mode)
    (parameterize ([current-output-port out])
      (write-string type-part)
      (define name (get-name this))
      (when name
        (write-string ":")
        (write-string (symbol->string name)))
      (write-string ">"))
    (void)))

(define (make-constant-custom-write name)
  (define str (string-append "#<" (symbol->string name) ">"))
  (λ (this out mode)
    (parameterize ([current-output-port out])
      (write-string str))
    (void)))
