#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [module-binding (-> module-path? phase? symbol? module-binding?)]
  [module-binding? predicate/c]
  [module-binding-source (-> module-binding? module-path?)]
  [module-binding-name (-> module-binding? symbol?)]
  [module-binding-phase (-> module-binding? phase?)]
  [module-bindings
   (->* (module-path?) (#:namespace namespace?) (set/c module-binding?))]
  [module-provided-bindings
   (->* (module-path?) (#:namespace namespace?) (set/c module-binding?))]
  [module-internal-bindings
   (->* (module-path?) (#:namespace namespace?) (set/c module-binding?))]))

(require racket/list
         racket/set
         rebellion/module/phase
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (dynamic-declare mod #:namespace [ns (current-namespace)])
  (parameterize ([current-namespace ns])
    (dynamic-require mod (void))))

(define-tuple-type module-binding (source phase name))

(define (module-provided-bindings mod #:namespace [ns (make-base-namespace)])
  (parameterize ([current-namespace ns])
    (dynamic-declare mod)
    (define-values (exported-variables exported-syntax) (module->exports mod))
    (for*/set ([export-list (in-list (list exported-variables exported-syntax))]
               [phase-export-list (in-list export-list)]
               [ph (in-value (phase (first phase-export-list)))]
               [export (in-list (rest phase-export-list))])
      (define name (first export))
      (module-binding mod ph name))))

(define (module-internal-bindings mod #:namespace [ns (make-base-namespace)])
  (parameterize ([current-namespace ns])
    (dynamic-declare mod)
    (for*/set ([phase-name-list (in-list (module->indirect-exports mod))]
               [ph (in-value (phase (first phase-name-list)))]
               [name (in-list (rest phase-name-list))])
      (module-binding mod ph name))))

(define (module-bindings mod #:namespace [ns (make-base-namespace)])
  (set-union (module-provided-bindings mod #:namespace ns)
             (module-internal-bindings mod #:namespace ns)))

(module+ test
  (test-case "module-provided-bindings"
    (define m 'rebellion/private/module-binding)
    (check-equal? (module-provided-bindings m)
                  (set
                   (module-binding m runtime-phase 'module-binding)
                   (module-binding m runtime-phase 'module-binding?)
                   (module-binding m runtime-phase 'module-binding-source)
                   (module-binding m runtime-phase 'module-binding-phase)
                   (module-binding m runtime-phase 'module-binding-name)
                   (module-binding m runtime-phase 'module-provided-bindings)
                   (module-binding m runtime-phase 'module-internal-bindings)
                   (module-binding m runtime-phase 'module-bindings)))))
