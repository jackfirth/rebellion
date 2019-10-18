#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [immutable-string? predicate/c]
  [immutable-string (-> char? ... immutable-string?)]
  [immutable-string-append (-> immutable-string? ... immutable-string?)]
  [make-immutable-string (->* (natural?) (char?) immutable-string?)]
  [immutable-string-length (-> immutable-string? natural?)]
  [immutable-string-ref (-> immutable-string? natural? char?)]
  [immutable-substring
   (->* (immutable-string? natural?) (natural?) immutable-string?)]
  [immutable-string->list (-> immutable-string? (listof char?))]
  [list->immutable-string (-> (listof char?) immutable-string?)]
  [build-immutable-string (-> natural? (-> natural? char?) immutable-string?)]
  [immutable-string=? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string<? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string<=? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string>? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string>=? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-ci=? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-ci<? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-ci<=? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-ci>? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-ci>=? (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-upcase (-> immutable-string? immutable-string?)]
  [immutable-string-downcase (-> immutable-string? immutable-string?)]
  [immutable-string-titlecase (-> immutable-string? immutable-string?)]
  [immutable-string-foldcase (-> immutable-string? immutable-string?)]
  [immutable-string-normalize-nfd (-> immutable-string? immutable-string?)]
  [immutable-string-normalize-nfkd (-> immutable-string? immutable-string?)]
  [immutable-string-normalize-nfc (-> immutable-string? immutable-string?)]
  [immutable-string-normalize-nfkc (-> immutable-string? immutable-string?)]
  [immutable-string-locale=?
   (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-locale<?
   (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-locale>?
   (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-locale-ci=?
   (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-locale-ci<?
   (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-locale-ci>?
   (-> immutable-string? immutable-string? ... boolean?)]
  [immutable-string-locale-upcase (-> immutable-string? immutable-string?)]
  [immutable-string-locale-downcase (-> immutable-string? immutable-string?)]
  [empty-immutable-string empty-immutable-string?]
  [empty-immutable-string? predicate/c]
  [nonempty-immutable-string? predicate/c]
  [symbol->immutable-string (-> symbol? immutable-string?)]
  [keyword->immutable-string (-> keyword? immutable-string?)]
  [immutable-string-join
   (->* ((listof immutable-string?))
        (immutable-string?
         #:before-first immutable-string?
         #:before-last immutable-string?
         #:after-last immutable-string?)
        immutable-string?)]
  [immutable-string-split
   (->* (immutable-string?)
        ((or/c immutable-string? regexp?)
         #:trim? boolean?
         #:repeat? boolean?)
        (listof immutable-string?))]
  [number->immutable-string
   (->* (number?) ((or/c 2 8 10 16)) immutable-string?)]))

(require (for-syntax racket/base
                     racket/string
                     racket/syntax)
         racket/math
         racket/string
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define (immutable-string? v) (and (string? v) (immutable? v)))
(define (empty-immutable-string? v) (and (equal? v "") (immutable? v)))

(define (nonempty-immutable-string? v)
  (and (immutable-string? v) (not (equal? v ""))))

(define empty-immutable-string "")

;@------------------------------------------------------------------------------

(define immutable-string-length string-length)
(define immutable-string-ref string-ref)
(define immutable-string->list string->list)
(define immutable-string=? string=?)
(define immutable-string<? string<?)
(define immutable-string<=? string<=?)
(define immutable-string>? string>?)
(define immutable-string>=? string>=?)
(define immutable-string-ci=? string-ci=?)
(define immutable-string-ci<? string-ci<?)
(define immutable-string-ci<=? string-ci<=?)
(define immutable-string-ci>? string-ci>?)
(define immutable-string-ci>=? string-ci>=?)
(define immutable-string-locale=? string-locale=?)
(define immutable-string-locale<? string-locale<?)
(define immutable-string-locale>? string-locale>?)
(define immutable-string-locale-ci=? string-locale-ci=?)
(define immutable-string-locale-ci<? string-locale-ci<?)
(define immutable-string-locale-ci>? string-locale-ci>?)

;@------------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class positional-argument
    (pattern id:id)
    (pattern [id:id default:expr]))

  (define-syntax-class string-function-header
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

(define-simple-macro
  (define-wrapper header:string-function-header)
  #:do [(define id-str (symbol->string (syntax-e #'header.id)))
        (define imm-str (string-replace id-str "string" "immutable-string"))]
  #:with immutable-id (datum->syntax #'header.id (string->symbol imm-str))
  (define immutable-id
    (λ header.lambda-argument-binders
      (string->immutable-string header.function-call-expression))))

(define-wrapper (string . chs))
(define-wrapper (string-append . strs))
(define-wrapper (make-string k [char #\nul]))
(define-wrapper (list->string lst))
(define-wrapper (build-string n proc))
(define-wrapper (string-upcase str))
(define-wrapper (string-downcase str))
(define-wrapper (string-titlecase str))
(define-wrapper (string-foldcase str))
(define-wrapper (string-locale-upcase str))
(define-wrapper (string-locale-downcase str))
(define-wrapper (string-normalize-nfd str))
(define-wrapper (string-normalize-nfkd str))
(define-wrapper (string-normalize-nfc str))
(define-wrapper (string-normalize-nfkc str))
(define-wrapper (symbol->string sym))
(define-wrapper (keyword->string kw))
(define-wrapper (number->string z [radix 10]))

(define (immutable-substring str start [end (immutable-string-length str)])
  (string->immutable-string (substring str start end)))

(define (immutable-string-join strs [sep " "]
                               #:before-first [before-first ""]
                               #:before-last [before-last sep]
                               #:after-last [after-last ""])
  (string->immutable-string
   (string-join strs sep
                #:before-first before-first
                #:before-last before-last
                #:after-last after-last)))

(define (immutable-string-split str
                                [sep #px"\\s+"]
                                #:trim? [trim? #t]
                                #:repeat? [repeat? #f])
  (define split-pieces (string-split str sep #:trim? trim? #:repeat? repeat?))
  (for/list ([piece (in-list split-pieces)]) (string->immutable-string piece)))
