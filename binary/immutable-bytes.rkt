#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [immutable-bytes? (-> any/c boolean?)]
  [make-immutable-bytes (-> natural? byte? immutable-bytes?)]
  [immutable-bytes (-> byte? ... immutable-bytes?)]
  [immutable-bytes-length (-> immutable-bytes? natural?)]
  [immutable-bytes-ref (-> immutable-bytes? natural? byte?)]
  [immutable-subbytes
   (->* (immutable-bytes? natural?) (natural?) immutable-bytes?)]
  [immutable-bytes-append (-> immutable-bytes? ... immutable-bytes?)]
  [immutable-bytes->list (-> immutable-bytes? (listof byte?))]
  [list->immutable-bytes (-> (listof byte?) immutable-bytes?)]
  [immutable-bytes=? bytes-comparison-function/c]
  [immutable-bytes<? bytes-comparison-function/c]
  [immutable-bytes>? bytes-comparison-function/c]
  [immutable-bytes->string/utf-8 bytes-encoder/c]
  [immutable-bytes->string/locale bytes-encoder/c]
  [immutable-bytes->string/latin-1 bytes-encoder/c]
  [immutable-string->bytes/utf-8 bytes-decoder/c]
  [immutable-string->bytes/locale bytes-decoder/c]
  [immutable-string->bytes/latin-1 bytes-decoder/c]
  [immutable-string-utf-8-length
   (->* (immutable-string?) (natural? natural?) natural?)]
  [immutable-bytes-utf-8-length
   (->* (immutable-bytes?) (err-char/c natural? natural?)
        (or/c natural? #f))]
  [immutable-bytes-utf-8-ref
   (->* (immutable-bytes? natural?) (err-char/c natural? natural?)
        (or/c char? #f))]
  [immutable-bytes-utf-8-index
   (->* (immutable-bytes? natural?) (err-char/c natural? natural?)
        (or/c natural? #f))]
  [immutable-bytes-convert
   (->* (bytes-converter? immutable-bytes?)
        (natural? natural?)
        (values immutable-bytes? natural? bytes-conversion-status/c))]
  [immutable-bytes-convert-end (-> bytes-converter? immutable-bytes?)]
  [immutable-bytes-append*
   (-> immutable-bytes? ... (listof immutable-bytes?) immutable-bytes?)]
  [immutable-bytes-join
   (-> (listof immutable-bytes?) immutable-bytes? immutable-bytes?)]
  [empty-immutable-bytes empty-immutable-bytes?]
  [empty-immutable-bytes? (-> any/c boolean?)]
  [nonempty-immutable-bytes? (-> any/c boolean?)]))

(require (for-syntax racket/base
                     racket/string)
         racket/bytes
         racket/math
         rebellion/base/immutable-string
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define (immutable-bytes? v) (and (bytes? v) (immutable? v)))

(define bytes-comparison-function/c
  (-> immutable-bytes? immutable-bytes? ... boolean?))

(define err-char/c (or/c char? #f))

(define bytes-encoder/c
  (->* (immutable-bytes?) (err-char/c natural? natural?)
       immutable-string?))

(define bytes-decoder/c
  (->* (immutable-string?) ((or/c byte? #f) natural? natural?)
       immutable-bytes?))

(define bytes-conversion-status/c
  (or/c 'complete 'continues 'aborts 'error))

(define empty-immutable-bytes #"")

(define (empty-immutable-bytes? v)
  (and (equal? v empty-immutable-bytes) (immutable? v)))

(define (nonempty-immutable-bytes? v)
  (and (immutable-bytes? v) (not (equal? v empty-immutable-bytes))))

;@------------------------------------------------------------------------------

(define immutable-bytes-length bytes-length)
(define immutable-bytes-ref bytes-ref)
(define immutable-bytes->list bytes->list)
(define immutable-bytes=? bytes=?)
(define immutable-bytes<? bytes<?)
(define immutable-bytes>? bytes>?)
(define immutable-string-utf-8-length string-utf-8-length)
(define immutable-bytes-utf-8-length bytes-utf-8-length)
(define immutable-bytes-utf-8-ref bytes-utf-8-ref)

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class positional-argument
    (pattern id:id)
    (pattern [id:id default:expr]))

  (define-syntax-class bytes-function-header
    #:attributes (id lambda-argument-binders function-call-expression)

    (pattern (id:id arg:positional-argument ...)
      #:with lambda-argument-binders #'(arg ...)
      #:with function-call-expression #'(id arg.id ...))

    (pattern (id:id . rest-arg:id)
      #:with lambda-argument-binders #'rest-arg
      #:with function-call-expression #'(apply id rest-arg))

    (pattern (id:id arg:positional-argument ...+ . rest-arg:id)
      #:with lambda-argument-binders #'(arg ... . rest-arg)
      #:with function-call-expression #'(apply id arg.id ... rest-arg))))

(define-syntax-parse-rule (define-wrapper header:bytes-function-header)
  #:do [(define id-str (symbol->string (syntax-e #'header.id)))
        (define imm-str (string-replace id-str "bytes" "immutable-bytes"))]
  #:with immutable-id (datum->syntax #'header.id (string->symbol imm-str))
  (define immutable-id
    (λ header.lambda-argument-binders
      (bytes->immutable-bytes header.function-call-expression))))

;@------------------------------------------------------------------------------

(define-wrapper (make-bytes size b))
(define-wrapper (bytes . bs))
(define-wrapper (bytes-append . bstrs))
(define-wrapper (list->bytes lst))
(define-wrapper (bytes-append* . bstrs+list))
(define-wrapper (bytes-join bstrs sep))

;@------------------------------------------------------------------------------

(define (immutable-subbytes bstr start [end (immutable-bytes-length bstr)])
  (bytes->immutable-bytes (subbytes bstr start end)))

(define ((make-encoder mutable-encoder)
         bstr [err-char #f] [start 0] [end (immutable-bytes-length bstr)])
  (string->immutable-string (mutable-encoder bstr err-char start end)))

(define ((make-decoder mutable-decoder)
         str [err-byte #f] [start 0] [end (immutable-string-length str)])
  (bytes->immutable-bytes (mutable-decoder str err-byte start end)))

(define immutable-bytes->string/utf-8 (make-encoder bytes->string/utf-8))
(define immutable-bytes->string/locale (make-encoder bytes->string/locale))
(define immutable-bytes->string/latin-1 (make-encoder bytes->string/latin-1))
(define immutable-string->bytes/utf-8 (make-decoder string->bytes/utf-8))
(define immutable-string->bytes/locale (make-decoder string->bytes/locale))
(define immutable-string->bytes/latin-1 (make-decoder string->bytes/latin-1))

(define (immutable-bytes-convert converter
                                 bstr
                                 [start 0]
                                 [end (immutable-bytes-length bstr)])
  (define-values (output-bstr amount-read? status)
    (bytes-convert converter bstr start end))
  (values (bytes->immutable-bytes output-bstr) amount-read? status))

(define bytes-convert-end-error-message
  (immutable-string-append
   "bytes converter ran out of space even though no destination bytestring was"
   " given to the converter, which should be impossible"))

(define (immutable-bytes-convert-end converter)
  (define-values (output-bstr status) (bytes-convert-end converter))
  (unless (equal? status 'complete)
    (raise-arguments-error 'immutable-bytes-convert-end
                           bytes-convert-end-error-message
                           "converter" converter
                           "status" status
                           "converter result bytes" output-bstr))
  (bytes->immutable-bytes output-bstr))

;; TODO(https://github.com/racket/racket/issues/2670): remove this wrapper since
;;   it's only needed in order to lie about bytes-utf-8-index's arity.
(define (immutable-bytes-utf-8-index bstr
                                     pos
                                     [err-byte #f]
                                     [start 0]
                                     [end (immutable-bytes-length bstr)])
  (bytes-utf-8-index bstr pos err-byte start end))
