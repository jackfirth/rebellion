#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [web-link
   (-> url-coercible? link-relation-coercible? url-coercible? web-link?)]
  [web-link? (-> any/c boolean?)]
  [web-link-source (-> web-link? url?)]
  [web-link-relation (-> web-link? (or/c url? symbol?))]
  [web-link-target (-> web-link? url?)]))

(require net/url
         racket/struct
         rebellion/type/tuple
         rebellion/equal+hash/tuple)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define url-coercible? (or/c url? string?))

(define (url-coerce url-ish)
  (if (string? url-ish) (string->url url-ish) url-ish))

(define link-relation-coercible? (or/c url? string? symbol?))

(define (link-relation-coerce relation-ish)
  (if (string? relation-ish) (string->url relation-ish) relation-ish))

(define (link-relation->writable-value relation)
  (if (symbol? relation) relation (url->string relation)))

(define (property-maker descriptor)
  (define name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define equal+hash (make-tuple-equal+hash descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this) (list (url->string (accessor this 0))
                     (link-relation->writable-value (accessor this 1))
                     (url->string (accessor this 2))))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-tuple-type web-link (source relation target)
  #:property-maker property-maker
  #:constructor constructor:web-link)

(define (web-link source relation target)
  (constructor:web-link (url-coerce source)
                        (link-relation-coerce relation)
                        (url-coerce target)))

(module+ test
  (test-case "prop:custom-write"
    (define link (web-link "http://example.org" 'stylesheet "/styles.css"))
    (check-equal? (~v link)
                  #<<END
(web-link "http://example.org" 'stylesheet "/styles.css")
END
                  )
    (check-equal? (~s link)
                  #<<END
#<web-link: "http://example.org" stylesheet "/styles.css">
END
                  )
    (check-equal? (~a link)
                  #<<END
#<web-link: http://example.org stylesheet /styles.css>
END
                  )))
