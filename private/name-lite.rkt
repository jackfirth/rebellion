#lang rebellion/private/dependencies

(provide
 (contract-out
  [atomic-name? (-> any/c boolean?)]
  [compound-name-leading-name (-> compound-name? symbolic-name?)]
  [compound-name-components
   (-> compound-name? (listof name-component?))]
  [compound-name? (-> any/c boolean?)]
  [name->s-expression (->* (name?) (#:unknown any/c) any/c)]
  [name-component? (-> any/c boolean?)]
  [name-literal (-> any/c name-literal?)]
  [name-literal-value (-> name-literal? any/c)]
  [name-literal? (-> any/c boolean?)]
  [name? (-> any/c boolean?)]
  [prop:name (struct-type-property/c (or/c #f symbol? name? (-> any/c name?)))]
  [prop:type-name
   (struct-type-property/c
    (or/c #f symbol? atomic-name? (-> any/c atomic-name?)))]
  [symbolic-name (-> symbol? symbolic-name?)]
  [symbolic-name? (-> any/c boolean?)]
  [symbolic-name->symbol (-> symbolic-name? symbol?)]
  [unknown-name unknown-name?]
  [unknown-name? (-> any/c boolean?)]
  [value-name (-> any/c name?)]
  [value-type-name (-> any/c name?)]
  [write-named-value (-> any/c output-port? (or/c #t #f 0 1) void?)]))

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-logger rebellion/name-lite)

(define (name? v) (or (atomic-name? v) (compound-name? v)))

;@------------------------------------------------------------------------------
;; atomic names

(define (atomic-name? v) (or (symbolic-name? v) (unknown-name? v)))

(define (write-unknown-name _ out mode)
  (write-string "#<unknown-name>" out)
  (void))

(struct unknown-name ()
  #:constructor-name plain-unknown-name
  #:omit-define-syntaxes
  #:transparent
  #:methods gen:custom-write [(define write-proc write-unknown-name)])

(define unknown-name (plain-unknown-name))

(struct symbolic-name (symbol-value)
  #:omit-define-syntaxes
  #:transparent)

(define (symbolic-name->symbol symname) (symbolic-name-symbol-value symname))

(define (write-symbolic-name-string symname out)
  (write-string (symbol->string (symbolic-name->symbol symname)) out)
  (void))

;@------------------------------------------------------------------------------
;; compound names

(define (name-component? v) (or (name? v) (name-literal? v)))

(struct compound-name (leading-name components)
  #:constructor-name plain-compound-name
  #:omit-define-syntaxes
  #:transparent)

(define (compound-name leading-name . components)
  (plain-compound-name leading-name components))

(struct name-literal (value)
  #:omit-define-syntaxes
  #:transparent)

(define (write-compound-name-string comname out mode)
  (write-string "(" out)
  (write-symbolic-name-string (compound-name-leading-name comname) out)
  (log-rebellion/name-lite-warning
   "compound name writing not yet implemented, cannot write value with name ~a"
   comname)
  (write-string ")" out)
  (void))

;@------------------------------------------------------------------------------
;; named values

(define name-property-guard #f)

(define (name-property-ref v property-predicate property-accessor)
  (cond [(not (property-predicate v)) unknown-name]
        [else
         (define name-prop (property-accessor v))
         (cond [(equal? name-prop #f) unknown-name]
               [(symbol? name-prop) (symbolic-name name-prop)]
               [(name? name-prop) name-prop]
               [else (name-prop v)])]))

(define-values (prop:name prop:name-predicate prop:name-accessor)
  (make-struct-type-property 'name name-property-guard))

(define-values (prop:type-name prop:type-name-predicate prop:type-name-accessor)
  (make-struct-type-property 'type-name name-property-guard))

(define (value-name v)
  (name-property-ref v prop:name-predicate prop:name-accessor))

(define (value-type-name v)
  (name-property-ref v prop:type-name-predicate prop:type-name-accessor))

(define (write-named-value v out mode)
  (define v-type-name (value-type-name v))
  (define v-name (value-name v))
  (write-string "#<" out)
  (if (unknown-name? v-type-name)
      (write-string "unknown" out)
      (write-symbolic-name-string v-type-name out))
  (unless (unknown-name? v-name)
    (write-string ":" out)
    (if (symbolic-name? v-name)
        (write-symbolic-name-string v-name out)
        (write-compound-name-string v-name out mode)))
  (write-string ">" out)
  (void))

;@------------------------------------------------------------------------------
;; named values integration test

(module+ test
  (test-case "integration-test"
    (struct widget (name)
      #:property prop:name (λ (v) (widget-name v))
      #:property prop:type-name 'widget
      #:methods gen:custom-write [(define write-proc write-named-value)])
    (define w (widget (symbolic-name 'frobnicator)))
    (check-equal? (value-name w) (symbolic-name 'frobnicator))
    (check-equal? (value-type-name w) (symbolic-name 'widget))
    (check-equal? (~v w) "#<widget:frobnicator>")
    (check-equal? (~a w) "#<widget:frobnicator>")
    (check-equal? (~s w) "#<widget:frobnicator>")
    (test-case "unknown-name-writing"
      (define w (widget unknown-name))
      (check-equal? (~v w) "#<widget>")
      (check-equal? (~a w) "#<widget>")
      (check-equal? (~s w) "#<widget>"))))

;@------------------------------------------------------------------------------
;; name->s-expression

(define (name->s-expression name #:unknown [unknown #f])
  (cond
    [(unknown-name? name) unknown]
    [(symbolic-name? name) (symbolic-name->symbol name)]
    [(compound-name? name)
     (define leading-sexp
       (symbolic-name->symbol (compound-name-leading-name name)))
     (let loop
       ([components (compound-name-components name)]
        [backwards-component-sexps (list leading-sexp)])
       (cond
         [(empty? components) (reverse backwards-component-sexps)]
         [else
          (define component (first components))
          (define component-sexp
            (if (name-literal? component)
                (name-literal-value component)
                (name->s-expression component #:unknown unknown)))
          (loop (rest components)
                (cons component-sexp backwards-component-sexps))]))]))

(module+ test
  (test-case "name->s-expression"
    (define name
      (compound-name (symbolic-name 'and/c)
                     (symbolic-name 'integer?)
                     (compound-name (symbolic-name 'not/c)
                                    (symbolic-name 'even?))
                     unknown-name
                     (compound-name (symbolic-name 'not/c)
                                    (name-literal 7))))
    (check-equal? (name->s-expression name #:unknown 'anonymous-contract)
                  (list 'and/c
                        'integer?
                        (list 'not/c 'even?)
                        'anonymous-contract
                        (list 'not/c 7)))))
