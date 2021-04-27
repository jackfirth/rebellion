#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [line-string (-> any/c ... immutable-string?)]
  [block-string (->* () (#:trailing-newline? boolean?) #:rest list? immutable-string?)]))


(require racket/format
         racket/string
         rebellion/base/immutable-string)


(define (line-string . parts)
  (string->immutable-string
   (string-trim (regexp-replace* #rx"\n+" (apply ~a parts) " ") #:repeat? #true)))


(define (block-string #:trailing-newline? [trailing-newline? #true] . parts)
  (define block (apply ~a parts))
  (define contains-non-whitespace?
    (for/or ([part (in-list parts)])
      (not (equal? part "\n"))))
  (string->immutable-string
   (if (and trailing-newline? contains-non-whitespace?) (string-append block "\n") block)))
