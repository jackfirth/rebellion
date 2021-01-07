#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [charset? predicate/c]
  [us-ascii charset?]
  [utf-8 charset?]
  [text? predicate/c]
  [text (-> (or/c charset? #f) immutable-bytes? text?)]
  [text-charset (-> text? (or/c charset? #f))]
  [text-bytes (-> text? immutable-bytes?)]
  [text-media? predicate/c]
  [text->media (-> text? text-media?)]
  [media->text (-> text-media? text?)]
  [text/plain (-> #:charset (or/c charset? #f) media-type?)]
  [text/plain? (-> media-type? boolean?)]))

(require racket/bool
         rebellion/binary/immutable-bytes
         rebellion/media
         rebellion/collection/record
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-tuple-type charset (name))

(define us-ascii (charset "us-ascii"))
(define utf-8 (charset "utf-8"))

(define-tuple-type text (charset bytes))

(define (text-media? m)
  (define type (media-get-type m))
  (equal? (media-type-top-level type) 'text))

(define (text->media txt)
  (media (text/plain #:charset (text-charset txt)) (text-bytes txt)))

(define (media->text m)
  (error 'media->text "not yet implemented"))

(define (text/plain? type)
  (and (equal? (media-type-top-level type) 'text)
       (equal? (media-type-subtype type) 'plain)
       (false? (media-type-tree type))
       (false? (media-type-suffix type))))

(define (text/plain #:charset chset)
  (define params
    (if chset (record #:charset (charset-name chset)) empty-record))
  (media-type 'text 'plain #:parameters params))
