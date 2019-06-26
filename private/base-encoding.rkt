#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [base64-encode (-> immutable-bytes? immutable-string?)]
  [base64url-encode (-> immutable-bytes? immutable-string?)]
  [hex-encode (-> immutable-bytes? immutable-string?)]))

(require racket/list
         rebellion/binary/bitstring
         rebellion/binary/byte
         rebellion/binary/immutable-bytes
         rebellion/immutable-string
         rebellion/type/record)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (byte-high-bits b n)
  (byte-drop-rightmost-bits b (- 8 n)))

(define (byte-low-bits b n)
  (byte-clear-leftmost-bits b (- 8 n)))

(define (make-bitstring size [filler-bit 0])
  (apply bitstring (make-list size filler-bit)))

(define (bitstring-append . bitstrings)
  (apply bitstring
         (for*/list ([bits (in-list bitstrings)]
                     [pos (in-range (bitstring-size bits))])
           (bitstring-ref bits pos))))

(define uppercase-ascii-letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define lowercase-ascii-letters "abcdefghijklmnopqrstuvwxyz")
(define ascii-digits "0123456789")

;@------------------------------------------------------------------------------

(define-record-type base-options
  (first-special-char second-special-char padding-char))

(define base64-options
  (base-options #:first-special-char #\+
                #:second-special-char #\/
                #:padding-char #\=))

(define base64url-options
  (base-options #:first-special-char #\-
                #:second-special-char #\_
                #:padding-char #\=))

(define (base-options-alphabet options)
  (define special1 (base-options-first-special-char options))
  (define special2 (base-options-second-special-char options))
  (immutable-string-append uppercase-ascii-letters
                           lowercase-ascii-letters
                           ascii-digits
                           (immutable-string special1 special2)))

(define (bitstring-pad-size-to-quantumn-size bits)
  (define size (bitstring-size bits))
  (unless (zero? (remainder size 8))
    (raise-arguments-error 'bitstring-pad-size-to-quantumn-size
                           "bitstring size not a multiple of eight"
                           "bitstring" bits
                           "size" size
                           "remainder" (remainder size 8)))
  (define r (remainder size 6))
  (define padding-size (if (zero? r) 0 (- 6 r)))
  (define padding-bits (make-bitstring padding-size))
  (bitstring-append bits padding-bits))

(module+ test
  (test-case "bitstring-pad-size-to-quantumn-size"
    (check-equal? (bitstring-pad-size-to-quantumn-size (bitstring))
                  (bitstring))
    (check-equal? (bitstring-pad-size-to-quantumn-size (make-bitstring 8 1))
                  (bitstring 1 1 1 1 1 1 1 1 0 0 0 0))
    (check-equal? (bitstring-pad-size-to-quantumn-size (make-bitstring 16 1))
                  (bitstring 1 1 1 1 1 1 1 1
                             1 1 1 1 1 1 1 1
                             0 0))))

(define (bitstring->quantumns bits)
  (define size (bitstring-size bits))
  (unless (zero? (remainder size 6))
    (raise-arguments-error 'bitstring->quantumns
                           "bitstring size not a multiple of six"
                           "bitstring" bits
                           "size" size
                           "remainder" (remainder size 6)))
  (define (ref base-pos offset)
    (bitstring-ref bits (+ base-pos offset)))
  (list->immutable-bytes
   (for/list ([pos (in-range 0 size 6)])
     (byte 0 0
           (ref pos 0)
           (ref pos 1)
           (ref pos 2)
           (ref pos 3)
           (ref pos 4)
           (ref pos 5)))))

(module+ test
  (test-case "bitstring->quantumns"
    (check-equal? (bitstring->quantumns (bitstring)) #"")
    (check-equal? (bitstring->quantumns (bitstring 1 1 1 1 1 1))
                  (immutable-bytes (byte 0 0 1 1 1 1 1 1)))))

(define (base-padding-string bstr options)
  (define pad-size
    (case (remainder (immutable-bytes-length bstr) 3)
      [(0) 0]
      [(1) 2]
      [(2) 1]))
  (make-immutable-string pad-size (base-options-padding-char options)))

(module+ test
  (test-case "base-padding-string"
    (check-equal? (base-padding-string #"" base64-options) "")
    (check-equal? (base-padding-string #"f" base64-options) "==")
    (check-equal? (base-padding-string #"fo" base64-options) "=")
    (check-equal? (base-padding-string #"foo" base64-options) "")
    (check-equal? (base-padding-string #"foob" base64-options) "==")
    (check-equal? (base-padding-string #"fooba" base64-options) "=")
    (check-equal? (base-padding-string #"foobar" base64-options) "")))

(define (base64-encode bstr #:options [options base64-options])
  (define alphabet (base-options-alphabet options))
  (define bits (bytes->bitstring bstr))
  (define padded-bits (bitstring-pad-size-to-quantumn-size bits))
  (define quantumns (bitstring->quantumns padded-bits))
  (define encoded-string
    (list->immutable-string
     (for/list ([q (in-bytes quantumns)])
       (immutable-string-ref alphabet q))))
  (define padding-string (base-padding-string bstr options))
  (immutable-string-append encoded-string padding-string))

(define (base64url-encode bstr)
  (base64-encode bstr #:options base64url-options))

(define hex-alphabet "0123456789ABCDEF")

(define (byte->hex b)
  (define pos1 (byte-high-bits b 4))
  (define pos2 (byte-low-bits b 4))
  (immutable-string (immutable-string-ref hex-alphabet pos1)
                    (immutable-string-ref hex-alphabet pos2)))

(define (hex-encode bstr)
  (apply immutable-string-append
         (for/list ([b (in-bytes bstr)])
           (byte->hex b))))

(module+ test
  (test-case "base64"
    (check-equal? (base64-encode #"") "")
    (check-equal? (base64-encode #"f") "Zg==")
    (check-equal? (base64-encode #"fo") "Zm8=")
    (check-equal? (base64-encode #"foo") "Zm9v")
    (check-equal? (base64-encode #"foob") "Zm9vYg==")
    (check-equal? (base64-encode #"fooba") "Zm9vYmE=")
    (check-equal? (base64-encode #"foobar") "Zm9vYmFy")
    (check-equal? (base64-encode (immutable-bytes (byte 1 1 1 1 1 0 1 1)
                                                  (byte 1 1 1 0 1 1 1 1)
                                                  (byte 1 0 1 1 1 1 1 0)))
                  "++++")
    (check-equal? (base64-encode (immutable-bytes 255 255 255)) "////"))
  (test-case "base64url"
    (check-equal? (base64url-encode #"") "")
    (check-equal? (base64url-encode #"f") "Zg==")
    (check-equal? (base64url-encode #"fo") "Zm8=")
    (check-equal? (base64url-encode #"foo") "Zm9v")
    (check-equal? (base64url-encode #"foob") "Zm9vYg==")
    (check-equal? (base64url-encode #"fooba") "Zm9vYmE=")
    (check-equal? (base64url-encode #"foobar") "Zm9vYmFy")
    (check-equal? (base64url-encode (immutable-bytes (byte 1 1 1 1 1 0 1 1)
                                                     (byte 1 1 1 0 1 1 1 1)
                                                     (byte 1 0 1 1 1 1 1 0)))
                  "----")
    (check-equal? (base64url-encode (immutable-bytes 255 255 255)) "____"))
  (test-case "hex"
    (check-equal? (hex-encode #"") "")
    (check-equal? (hex-encode #"f") "66")
    (check-equal? (hex-encode #"fo") "666F")
    (check-equal? (hex-encode #"foo") "666F6F")
    (check-equal? (hex-encode #"foob") "666F6F62")
    (check-equal? (hex-encode #"fooba") "666F6F6261")
    (check-equal? (hex-encode #"foobar") "666F6F626172")))
