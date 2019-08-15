#lang parendown racket/base

(require racket/contract)

(provide #/contract-out
  [application/octet-stream (->* () (#:padding (integer-in 0 7)) media-type?)]
  [media->octet-stream (-> media? octet-stream?)]
  [octet-stream
   (->* (immutable-bytes?) (#:padding (integer-in 0 7)) octet-stream?)]
  [octet-stream? predicate/c]
  [octet-stream-bytes (-> octet-stream? immutable-bytes?)]
  [octet-stream-padding (-> octet-stream? (integer-in 0 7))]
  [octet-stream->bitstring (-> octet-stream? bitstring?)]
  [octet-stream->media (-> octet-stream? media?)])

(require (only-in lathe-comforts expect fn mat w-)
         (only-in lathe-comforts/maybe just)
         rebellion/binary/bitstring
         rebellion/binary/immutable-bytes
         rebellion/collection/record
         rebellion/base/immutable-string
         rebellion/media
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/binary/byte))

;@------------------------------------------------------------------------------

;; TODO: handle the "type" parameter
(define-tuple-type octet-stream (bytes padding)
  #:constructor-name plain-octet-stream)

(define (octet-stream bytes #:padding [padding 0])
  (plain-octet-stream bytes padding))

(define (dissect-octet-stream octets body)
  (body (octet-stream-bytes octets) (octet-stream-padding octets)))

(define (octet-stream->bitstring octets)
  (dissect-octet-stream octets #/fn bytes padding
  #/bytes->bitstring bytes #:padding padding))

(define (application/octet-stream #:padding [padding 0])
  (mat padding 0
    (media-type 'application 'octet-stream)
    (media-type 'application 'octet-stream
                #:parameters
                (record #:padding #/number->immutable-string padding))))

(define (application/octet-stream? type)
  (and (equal? 'application #/media-type-top-level type)
       (equal? 'octet-stream #/media-type-subtype type)
       (not #/media-type-tree type)
       (not #/media-type-suffix type)))

(define (media->octet-stream m)
  (w- bstr (media-bytes m)
  #/w- type (media-get-type m)
  #/w- padding
    (expect (application/octet-stream? type) #t 0
    #/w- params (media-type-parameters type)
    #/expect (record-ref-maybe params '#:padding) (just padding) 0
      padding)
  #/octet-stream bstr #:padding padding))

(define (octet-stream->media octets)
  (dissect-octet-stream octets #/fn bytes padding
  #/media (application/octet-stream #:padding padding) bytes))

(module+ test
  (test-case "octet-stream->bitstring"
    (define bstr
      (bytes (byte 1 1 0 0 0 1 1 1)
             (byte 0 0 1 0 0 0 1 1)
             (byte 1 1 0 0 0 0 0 0)))
    (define octets (octet-stream (bytes->immutable-bytes bstr) #:padding 3))
    (check-equal? (octet-stream->bitstring octets)
                  (bitstring 1 1 0 0 0 1 1 1
                             0 0 1 0 0 0 1 1
                             1 1 0 0 0))))
