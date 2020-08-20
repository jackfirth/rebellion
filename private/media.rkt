#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [media (-> media-type? immutable-bytes? media?)]
  [media? predicate/c]
  [media-bytes (-> media? immutable-bytes?)]
  [media-get-type (-> media? media-type?)]
  [media-type
   ;; TODO: improve checking and handling of allowed characters / length
   ;;   restrictions / case sensitivity / etc. and put an actual contract on the
   ;;   field values of the parameters record
   (->* (interned-symbol? interned-symbol?)
        (#:tree (or/c interned-symbol? #f)
         #:suffix (or/c interned-symbol? #f)
         #:parameters record?)
        media-type?)]
  [media-type? predicate/c]
  [media-type-top-level (-> media-type? interned-symbol?)]
  [media-type-subtype (-> media-type? interned-symbol?)]
  [media-type-tree (-> media-type? (or/c interned-symbol? #f))]
  [media-type-suffix (-> media-type? (or/c interned-symbol? #f))]
  [media-type-parameters (-> media-type? record?)]
  [media-type->string (-> media-type? immutable-string?)]
  [string->media-type (-> immutable-string? media-type?)]))

(require racket/string
         racket/struct
         rebellion/base/immutable-string
         rebellion/base/symbol
         rebellion/binary/immutable-bytes
         rebellion/collection/keyset
         rebellion/collection/record
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------
;; media-type
;; TODO: use better representations of top-level types, trees, and structured
;;   suffixes, instead of just allowing arbitrary symbols

(define (make-media-type-properties descriptor)
  (define name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define equal+hash (default-tuple-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this) (list (media-type->string this)))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-tuple-type media-type (top-level tree subtype suffix parameters)
  #:omit-root-binding
  #:property-maker make-media-type-properties)

(define (media-type top-level subtype
                    #:tree [tree #f]
                    #:suffix [suffix #f]
                    #:parameters [parameters empty-record])
  ;; TODO: media type instances should probably be interned
  (constructor:media-type top-level tree subtype suffix parameters))

;; TODO: figure out how to correctly deal with the fact that RFC 6838 doesn't
;;   actually say how parameter values should be parsed or serialized, and it
;;   doesn't even assume that parameter values are strings (as opposed to
;;   arbitrary binary data)
(define (media-type->string type)
  (define top-level (media-type-top-level type))
  (define subtype (media-type-subtype type))
  (define tree (media-type-tree type))
  (define suffix (media-type-suffix type))
  (define parameters (media-type-parameters type))
  (immutable-string-append
   (symbol->immutable-string top-level)
   "/"
   (if tree (immutable-string-append (symbol->immutable-string tree) ".") "")
   (symbol->immutable-string subtype)
   (if suffix
       (immutable-string-append "+" (symbol->immutable-string suffix))
       "")
   (media-type-parameters->string parameters)))

(define (media-type-parameters->string parameters)
  (define parameter-keys (record-keywords parameters))
  (define param-strings
    (for/list ([kw (in-keyset parameter-keys)])
      (define param-raw (record-ref parameters kw))
      (define param-name (keyword->immutable-string kw))
      (define param-quoted
        (media-type-parameter-value->possibly-quoted-string param-raw))
      (immutable-string-append param-name "=" param-quoted)))
  (define sep "; ")
  (define joined (immutable-string-join param-strings sep))
  (if (nonempty-immutable-string? joined)
      (immutable-string-append sep joined)
      joined))

(define (media-type-parameter-value->possibly-quoted-string param-value-string)
  ;; TODO: add backslash quoting (see RFC 7230 Section 3.2.6)
  (if (or (for/or ([c (in-string param-value-string)])
            (media-type-parameter-value-char-requires-quoting? c))
          (empty-immutable-string? param-value-string))
      (immutable-string-append "\"" param-value-string "\"")
      param-value-string))

(define (media-type-parameter-value-char-requires-quoting? char)
  ;; TODO: use a better data structure here
  (not (string-contains? rfc7230-section-3.2.6-tchar-legal-alphabet
                         (immutable-string char))))

(define rfc7230-section-3.2.6-tchar-legal-alphabet
  (immutable-string-append "abcdefghijklmnopqrstuvwxyz"
                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                           "0123456789"
                           "!#$%&'*+-.^_`|~"))

(define (string->media-type s)
  ;; TODO: implement media-type parser based on HTTP Content-Type header grammar
  (error 'unimplemented))

(module+ test
  (test-case "media-type-with-only-required-parts"
    (define mt (media-type 'video 'example))
    (check-equal? (media-type->string mt) "video/example"))
  (test-case "media-type-with-tree"
    (define mt (media-type 'application 'coffeescript #:tree 'vnd))
    (check-equal? (media-type->string mt) "application/vnd.coffeescript"))
  (test-case "media-type-with-one-unquoted-param"
    (define mt
      (media-type 'text 'plain #:parameters (record #:charset "utf-8")))
    (check-equal? (media-type->string mt) "text/plain; charset=utf-8"))
  (test-case "media-type-with-one-quoted-param"
    (define mt
      (media-type 'application 'example
                  #:parameters (record #:param "this param has whitespace")))
    (check-equal? (media-type->string mt)
                  "application/example; param=\"this param has whitespace\""))
  (test-case "media-type-with-multiple-params"
    (define mt
      (media-type 'text 'example
                  #:parameters (record #:charset "utf-8"
                                       #:other-param "other-value")))
    (check-equal? (media-type->string mt)
                  "text/example; charset=utf-8; other-param=other-value"))
  (test-case "media-type-printing"
    (define mt
      (media-type 'text 'example #:parameters (record #:charset "utf-8")))
    (check-equal? (~v mt) "(media-type \"text/example; charset=utf-8\")")))

;@------------------------------------------------------------------------------
;; media

(define type:media (tuple-type 'media (list 'type 'bytes)))
(define descriptor:media (make-tuple-implementation type:media))
(define media (tuple-descriptor-constructor descriptor:media))
(define media? (tuple-descriptor-predicate descriptor:media))

;; TODO(https://github.com/jackfirth/rebellion/issues/75): improve type
;;   definition macros so this isn't necessary anymore
(define media-get-type
  (procedure-rename
   (make-tuple-field-accessor descriptor:media 0)
   'media-get-type))

(define media-bytes (make-tuple-field-accessor descriptor:media 1))

(module+ test
  (test-case "media"
    (define text/plain
      (media-type 'text 'plain #:parameters (record #:charset "utf-8")))
    (define m (media text/plain #"hello world"))
    (check-equal? (media-get-type m) text/plain)
    (check-equal? (media-bytes m) #"hello world")))
