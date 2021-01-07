#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [application/octet-stream (->* () (#:padding (integer-in 0 7)) media-type?)]
  [media->octet-stream (-> media? octet-stream?)]
  [octet-stream
   (->* (immutable-bytes?) (#:padding (integer-in 0 7)) octet-stream?)]
  [octet-stream? predicate/c]
  [octet-stream-bytes (-> octet-stream? immutable-bytes?)]
  [octet-stream-padding (-> octet-stream? (integer-in 0 7))]
  [octet-stream->bitstring (-> octet-stream? bitstring?)]
  [octet-stream->media (-> octet-stream? media?)]))

(require rebellion/base/immutable-string
         rebellion/binary/bitstring
         rebellion/binary/immutable-bytes
         rebellion/collection/record
         rebellion/media
         rebellion/private/guarded-block
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/binary/byte))

;@------------------------------------------------------------------------------

;; TODO: handle the "type" parameter
(define-tuple-type octet-stream (bytes padding) #:omit-root-binding)

(define (octet-stream bytes #:padding [padding 0])
  (constructor:octet-stream bytes padding))

(define (octet-stream->bitstring octets)
  (bytes->bitstring (octet-stream-bytes octets)
                    #:padding (octet-stream-padding octets)))

(define (application/octet-stream #:padding [padding 0])
  (if (zero? padding)
      (media-type 'application 'octet-stream)
      (media-type 'application 'octet-stream
                  #:parameters
                  (record #:padding (number->immutable-string padding)))))

(define (application/octet-stream? type)
  (and (equal? (media-type-top-level type) 'application)
       (equal? (media-type-subtype type) 'octet-stream)
       (not (media-type-tree type))
       (not (media-type-suffix type))))

(define (media->octet-stream m)
  (define bstr (media-bytes m))
  (define type (media-get-type m))
  (define padding
    (guarded-block
      (guard (application/octet-stream? type) else 0)
      (define params (media-type-parameters type))
      (if (record-contains-key? params '#:padding)
          (string->number (record-ref params '#:padding))
          0)))
  (octet-stream bstr #:padding padding))

(define (octet-stream->media octets)
  (media (application/octet-stream #:padding (octet-stream-padding octets))
         (octet-stream-bytes octets)))

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
