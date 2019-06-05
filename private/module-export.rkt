#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [export? (-> any/c boolean?)]
  [export-name (-> export? symbol?)]
  [export-phase (-> export? natural?)]
  [export-origin? (-> any/c boolean?)]
  [export-origin-source-module (-> export-origin? module-path-index?)]
  [export-origin-phase (-> export-origin? natural?)]
  [export-origin-phase-shift (-> export-origin? natural?)]
  [export-origin-imported-alias (-> export-origin? symbol?)]
  [export-origins (-> export? (set/c export-origin? #:cmp 'equal))]
  [module-exports (-> module-path? (set/c export? #:cmp 'equal))]
  [syntax-export? (-> any/c boolean?)]
  [variable-export? (-> any/c boolean?)]))

(require (for-syntax racket/base
                     racket/syntax)
         racket/bool
         racket/list
         racket/math
         racket/set
         racket/struct
         rebellion/equal+hash/tuple
         rebellion/type/tuple
         syntax/parse/define)

;@------------------------------------------------------------------------------

(define empty-set (set))

;@------------------------------------------------------------------------------

(define (make-export-properties descriptor)
  (define equal+hash (make-tuple-equal+hash descriptor))
  (define name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this)
       (define export-name (accessor this 0))
       (define phase (accessor this 1))
       (define origins (accessor this 2))
       (append (list export-name)
               (if (zero? phase)
                   empty
                   (list (unquoted-printing-string "#:phase") phase))
               (if (set-empty? origins)
                   empty
                   (list (unquoted-printing-string "#:origins") origins))))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define (make-origin-properties descriptor)
  (define equal+hash (make-tuple-equal+hash descriptor))
  (define name (tuple-type-name (tuple-descriptor-type descriptor)))
  (define accessor (tuple-descriptor-accessor descriptor))
  (define custom-write
    (make-constructor-style-printer
     (λ (_) name)
     (λ (this)
       (define source-module (accessor this 0))
       (define phase (accessor this 1))
       (define phase-shift (accessor this 2))
       (define imported-alias (accessor this 3))
       (append (list source-module)
               (if (zero? phase)
                   empty
                   (list (unquoted-printing-string "#:phase") phase))
               (if (zero? phase-shift)
                   empty
                   (list (unquoted-printing-string "#:phase-shift")
                         phase-shift))
               (if (false? imported-alias)
                   empty
                   (list (unquoted-printing-string "#:imported-alias")
                         imported-alias))))))
  (list (cons prop:equal+hash equal+hash)
        (cons prop:custom-write custom-write)))

(define-tuple-type variable-export (name phase origins)
  #:constructor-name constructor:variable-export
  #:property-maker make-export-properties)

(define-tuple-type syntax-export (name phase origins)
  #:constructor-name constructor:syntax-export
  #:property-maker make-export-properties)

(define-tuple-type export-origin
  (source-module phase phase-shift imported-alias)
  #:constructor-name constructor:export-origin
  #:property-maker make-origin-properties)

(define (variable-export name #:phase [phase 0] #:origins [origins empty-set])
  (constructor:variable-export name phase origins))

(define (syntax-export name #:phase [phase 0] #:origins [origins empty-set])
  (constructor:syntax-export name phase origins))

(define (export-origin mpi
                       #:phase [phase 0]
                       #:phase-shift [shift 0]
                       #:new-name [new-name #f])
  (constructor:export-origin mpi phase shift new-name))

(define (export? v) (or (variable-export? v) (syntax-export? v)))

(define (export-case export #:variable f #:syntax g)
  (if (variable-export? export) (f export) (g export)))

(define (export-name export)
  (export-case export
               #:variable variable-export-name
               #:syntax syntax-export-name))

(define (export-phase export)
  (export-case export
               #:variable variable-export-phase
               #:syntax syntax-export-phase))

(define (export-origins export)
  (export-case export
               #:variable variable-export-origins
               #:syntax syntax-export-origins))

(define (module-exports modpath)
  (dynamic-require modpath #f)
  (define-values (exported-variables exported-syntax) (module->exports modpath))
  (define (make-exports maker exported-lst)
    (for*/set ([phase-variables (in-list exported-lst)]
               [phase (in-value (first phase-variables))]
               [variable-origins (in-list (rest phase-variables))]
               [variable (in-value (first variable-origins))])
      (define origins
        (for/set ([origin (in-list (second variable-origins))])
          (if (module-path-index? origin)
              (export-origin origin)
              (export-origin (first origin)
                             #:phase (second origin)
                             #:phase-shift (fourth origin)
                             #:new-name (third origin)))))
      (maker variable #:phase phase #:origins origins)))
  (set-union (make-exports variable-export exported-variables)
             (make-exports syntax-export exported-syntax)))
