#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [package? predicate/c]
  [package (-> #:name immutable-string?
               #:short-name interned-symbol?
               #:catalog immutable-string?
               package?)]
  [package-name (-> package? immutable-string?)]
  [package-short-name (-> package? interned-symbol?)]
  [package-catalog (-> package? immutable-string?)]
  [get-catalog-packages (-> immutable-string? (immutable-set/c package?))]))

(require json
         net/url
         racket/port
         racket/set
         rebellion/immutable-string
         rebellion/media
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define (interned-symbol? v) (and (symbol? v) (symbol-interned? v)))

(define (immutable-set/c elem-contract)
  (set/c elem-contract #:cmp 'equal #:kind 'immutable))

(define (port->immutable-bytes port)
  (bytes->immutable-bytes (port->bytes port)))

(define-record-type content (location type bytes))
(define-record-type content-descriptor (type deserializer))

(define (http-get url descriptor)
  (define type (content-descriptor-type descriptor))
  (define headers
    (list (immutable-string-append "Accept: " (media-type->string type))))
  ;; This completely ignores status codes and the actual Content-Type
  ;; header of the response, but oh well
  (define response-body
    (call/input-url url get-pure-port port->immutable-bytes headers))
  (define deserializer (content-descriptor-deserializer descriptor))
  (deserializer (content #:location url
                         #:type type
                         #:bytes response-body)))

(define (build-relative-url base . relative-paths)
  (for/fold ([base base])
            ([path (in-list relative-paths)])
    (combine-url/relative base path)))

;@------------------------------------------------------------------------------

(define-record-type package (name short-name catalog))

(define (pkgs-all-deserialize cont)
  (define pkgs-all-url (content-location cont))
  (define pkgs-all (with-input-from-bytes (content-bytes cont) read))
  (for/list ([(short-name details) (in-hash pkgs-all)])
    (define name
      (build-relative-url pkgs-all-url
                          "../pkg"
                          short-name))
    (package #:name (url->string name)
             #:short-name short-name
             #:catalog (url->string (build-relative-url pkgs-all-url "..")))))

(define pkgs-all-content
  (content-descriptor #:type (media-type 'application 's-exp #:tree 'vnd)
                      #:deserializer pkgs-all-deserialize))

(define (get-catalog-packages catalog)
  (define url (build-relative-url (string->url catalog) "pkgs-all"))
  (http-get url pkgs-all-content))

(define plt-package-catalog "https://pkgs.racket-lang.org")
(define planet-compat-package-catalog "http://planet-compats.racket-lang.org")
