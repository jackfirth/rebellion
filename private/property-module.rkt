#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [property-module (-> property-binding? ... property-module?)]
  [empty-property-module property-module?]
  [property-module? predicate/c]
  [property-module-add-binding
   (-> property-module? property-binding? property-module?)]
  [property-module-outputs
   (-> property-module? (immutable-set/c struct-type-property?))]
  [property-module-instantiate
   (-> property-module? property-hash/c)]
  [property-binding
   (-> struct-type-property? procedure? struct-type-property? ...
       property-binding?)]
  [constant-property-binding
   (-> struct-type-property? any/c property-binding?)]
  [property-binding? predicate/c]
  [property-binding-output
   (-> property-binding? struct-type-property?)]
  [make-hidden-type-property (-> symbol? struct-type-property?)]))

(require racket/set)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (immutable-set/c element-contract)
  (set/c element-contract #:cmp 'equal #:kind 'immutable #:lazy? #f))

(define property-hash/c (hash/c struct-type-property? any/c))

;@------------------------------------------------------------------------------

(define (make-hidden-type-property name)
  (define-values (property ignored-predicate ignored-accessor)
    (make-struct-type-property name))
  property)

(struct property-binding (function output dependencies)
  #:omit-define-syntaxes
  #:constructor-name constructor:property-binding)

(define (make-raw-property-binding #:function function
                                   #:output output
                                   #:dependencies dependencies)
  (constructor:property-binding function output dependencies))

(define (constant-property-binding prop v)
  (make-raw-property-binding #:function (λ (_) v)
                             #:output prop
                             #:dependencies (set)))

(define (property-binding prop function . dependencies)
  (define (raw-function bound-deps)
    (apply function (map (λ (dep) (hash-ref bound-deps dep)) dependencies)))
  (make-raw-property-binding #:function raw-function
                             #:output prop
                             #:dependencies (list->set dependencies)))

(struct property-module (bindings outputs dependencies)
  #:omit-define-syntaxes
  #:constructor-name constructor:property-module)

(define (make-property-module #:bindings bindings
                              #:outputs outputs
                              #:dependencies dependencies)
  (unless (list? bindings)
    (raise-argument-error 'make-property-module "list?" bindings))
  (unless (set? outputs)
    (raise-argument-error 'make-property-module "set?" outputs))
  (unless (set? dependencies)
    (raise-argument-error 'make-property-module "set?" dependencies))
  (constructor:property-module bindings outputs dependencies))

(define empty-property-module
  (make-property-module #:bindings '() #:outputs (set) #:dependencies (set)))

(define (property-module-add-binding propmod binding)
  (define outputs (property-module-outputs propmod))
  (define deps (property-module-dependencies propmod))
  (define binding-output (property-binding-output binding))
  (define binding-deps (property-binding-dependencies binding))
  (when (set-member? outputs binding-output)
    (raise-arguments-error
     'property-module-add-binding
     "cannot add binding for type property, it is already bound"
     "type property" binding-output
     "binding" binding
     "property module" propmod))
  (define depends-on? (not (set-empty? (set-intersect binding-deps outputs))))
  (define dependency-of? (set-member? deps binding-output))
  (when (and depends-on? dependency-of?)
    (raise-arguments-error
     'property-module-add-binding
     (string-append "cannot add binding for type property, binding and property"
                    " module have circular dependency")
     "type property" binding-output
     "binding" binding
     "property module" propmod))
  (define new-outputs (set-add outputs binding-output))
  (cond
    [depends-on?
     (define new-bindings
       (append (property-module-bindings propmod) (list binding)))
     (define new-deps (set-union deps (set-subtract binding-deps outputs)))
     (make-property-module #:bindings new-bindings
                           #:outputs new-outputs
                           #:dependencies new-deps)]
    [dependency-of?
     (define new-bindings (cons binding (property-module-bindings propmod)))
     (define new-deps (set-union binding-deps (set-remove deps binding-output)))
     (make-property-module #:bindings new-bindings
                           #:outputs new-outputs
                           #:dependencies new-deps)]
    [else
     (define new-bindings (cons binding (property-module-bindings propmod)))
     (define new-deps (set-union binding-deps deps))
     (make-property-module #:bindings new-bindings
                           #:outputs new-outputs
                           #:dependencies new-deps)]))

(define (property-module . bindings)
  (for/fold ([mod empty-property-module])
            ([b (in-list bindings)])
    (property-module-add-binding mod b)))

(define (property-module-instantiate mod)
  (for/fold ([h (hash)])
            ([b (in-list (property-module-bindings mod))])
    (define binding-func (property-binding-function b))
    (hash-set h (property-binding-output b) (binding-func h))))

(module+ test
  (define-values (prop:foo has-foo? value-foo) (make-struct-type-property 'foo))
  (define-values (prop:bar has-bar? value-bar) (make-struct-type-property 'bar))
  (define-values (prop:baz has-baz? value-baz) (make-struct-type-property 'baz))
  (test-case "integration-test"
    (define mod
      (property-module
       (constant-property-binding prop:foo 10)
       (property-binding prop:bar / prop:foo prop:baz)
       (constant-property-binding prop:baz 2)))
    (check-equal? (property-module-outputs mod)
                  (set prop:foo prop:bar prop:baz))
    (check-equal? (property-module-dependencies mod) (set))
    (check-equal? (property-module-instantiate mod)
                  (hash prop:foo 10
                        prop:bar 5
                        prop:baz 2))))
