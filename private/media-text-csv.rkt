#lang racket/base

(require racket/contract/base)

;; TODO: provide constructors for CSV types
;; TODO: provide converters between rebellion/table and CSV
(provide
 (contract-out
  [text/csv (->* ()
                 (#:charset (or/c charset? #f)
                  #:header (or/c 'present 'absent #f))
                 media-type?)]
  [csv? predicate/c]
  [csv-fragment-identifier? predicate/c]
  ;; TODO: use rebellion/converter to define csv<->media
  [media->csv (-> media? csv?)]
  [csv->media (-> csv? media?)]))

(require racket/list
         racket/string
         rebellion/media
         rebellion/record
         rebellion/tuple-type-definition)

;@------------------------------------------------------------------------------
;; utils

;; TODO: move to rebellion/media/text module
;; TODO: handle more charsets
(define (charset? v) (equal? v "utf-8"))

;; TODO: move to rebellion/media/text module
(define (media->string med)
  (define type (media-get-type med))
  (define bstr (media-bytes med))
  (define params (media-type-parameters type))
  ;; TODO: figure out a nicer way to handle defaults in rebellion/record, or
  ;;   consider changing media type params to use a hash instead of a record
  (define params/default (record-merge2 (record #:charset "utf-8") params))
  (define charset (record-ref params/default '#:charset))
  (case charset
    [("utf-8") (bytes->string/utf-8 bstr)]
    [else (error 'media->text "charset ~v not implemented" charset)]))

;; TODO: move to rebellion/list module
(define empty-list (list))

;; TODO: move to rebellion/string module
(define (empty-string? v) (equal? v ""))

;; TODO: move this somewhere more general
(define CR (integer->char 13))
(define LF (integer->char 10))
(define CRLF (string CR LF))

;; A block parser is a function for non-incremental string parsing that accepts
;; a string and optional start and end indices, then returns a list of possible
;; parse results. Block parsers require the entire input string to be in memory
;; and immutable, and are expected to avoid allocating new strings by passing
;; around start and end indices and implicitly assuming all indices are relative
;; to the original input string. It is conventional to add the suffix "/bp" to
;; block parsers and functions that create block parsers.
;; TODO: move this to its own module and add docs, but make sure it's clear to
;;   users that it's a private module used internally by Rebellion and not meant
;;   for external use.
(struct block-parser (function))

(define-tuple-type block-parse-result (handler size))

(define (block-parse p str start end)
  (define f (block-parser-function p))
  (for/list ([result (in-list (f str start end))])
    (define handler (block-parse-result-handler result))
    (define size (block-parse-result-size result))
    (define substr (substring str start (+ start size)))
    (handler substr)))

(define (success/bp v)
  (block-parser
   (λ (str start end)
     (list (block-parse-result (λ (_) v) 0)))))

(define (consume/bp size)
  (block-parser
   (λ (str start end)
     (if (>= (- end start) size)
         (list (block-parse-result values size))
         empty-list))))

(define (map/bp parser f)
  (define parser-function (block-parser-function parser))
  (block-parser
   (λ (str start end)
     (for/list ([result (in-list (parser-function str start end))])
       (define handler (block-parse-result-handler result))
       (define size (block-parse-result-size result))
       (block-parse-result (compose1 f handler) size)))))

;; TODO: implement this, it's the Applicative instance on parsers and it should
;;   sequence them all together with requirement that they all succeed
(define (zip/bp f . parsers)
  (define parser-functions (map block-parser-function parsers))
  (block-parser
   (λ (str start end)
     (error 'zip/bp "unimplemented"))))

;; TODO: choosing between alternate parsers (Alternative impl)
;; TODO: Monad impl? maybe?
;; TODO: something like (consume-exact/bp "foo") for matching and discarding
;;   things like separators and punctuation
;; TODO: something like (while/bp (λ (char) ...)) for consuming chars
;;   matching a predicate
;; TODO: something like (chars-in/bp "aeiou") and (consume-chars-in/bp "aeiou")

;@------------------------------------------------------------------------------
;; implementation of text/csv media type, as specified in RFCs 4180 and 7111

(define-tuple-type csv (header rows))

(define (text/csv #:charset [charset "utf-8"] #:header [header #f])
  (define header-str (and header (symbol->string header)))
  (define params
    (cond
      [(and charset header-str) (record #:charset charset #:header header-str)]
      [charset (record #:charset charset)]
      [header-str (record #:header header-str)]
      [else empty-record]))
  (media-type 'text 'csv #:parameters params))

;; TODO: this should use block parsers, still working on implementing those
(define (media->csv med)
  (define type (media-get-type med))
  (define str (media->string med))
  ;; CSV files are allowed to have an optional trailing newline, and this
  ;; doesn't count as an extra row.
  (define lines/maybe-empty-last-line (string-split str CRLF #:trim? #f))
  (define lines
    (if (empty-string? (last lines/maybe-empty-last-line))
        (drop-right lines/maybe-empty-last-line 1)
        lines/maybe-empty-last-line))
  (error 'media->csv "unimplemented"))

;; TODO: implement this
(define (csv->media csv)
  (error 'csv->media "unimplemented"))

;@------------------------------------------------------------------------------
;; fragment identifiers (e.g. #row=5, #cell=4,1)

;; TODO: implement this
(define (csv-fragment-identifier? _) #f)
