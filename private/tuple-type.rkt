#lang racket/base

(provide define-tuple-type)

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         racket/match
         rebellion/type/tuple/base
         rebellion/type/tuple/descriptor
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))

;@------------------------------------------------------------------------------

(define-simple-macro
  (define-tuple-type id:id (field:id ...)
    (~alt
     (~optional (~and #:omit-root-binding omit-root-binding-kw))
     (~optional
      (~seq #:constructor-name constructor:id)
      #:defaults
      ([constructor (format-id #'id "constructor:~a" #'id #:subs? #t)]))
     (~optional (~seq #:predicate-name predicate:id)
                #:defaults ([predicate (format-id #'id "~a?" #'id #:subs? #t)]))
     (~optional (~seq #:property-maker property-maker:expr)
                #:defaults ([property-maker #'default-tuple-properties]))
     (~optional
      (~seq #:pattern-name pattern:id)
      #:defaults ([pattern (format-id #'id "pattern:~a" #'id #:subs? #t)])))
    ...)
  #:do [(define size (length (syntax->list #'(field ...))))]
  #:with quoted-size #`(quote #,size)
  #:with (field-accessor ...)
  (for/list ([field-id (in-syntax #'(field ...))])
    (format-id field-id "~a-~a" #'id field-id #:subs? #t))
  #:with (field-position ...) (build-list size (λ (n) #`(quote #,n)))
  #:with root-binding
  (if (attribute omit-root-binding-kw)
      #'(begin)
      #'(...
         (define-match-expander id
           (syntax-parser [(_ rest ...) #'(pattern rest ...)])
           (make-rename-transformer #'constructor))))
  (begin
    (define descriptor
      (make-tuple-implementation
       (tuple-type 'id quoted-size
                   #:constructor-name 'constructor
                   #:predicate-name 'predicate)
       #:property-maker property-maker))
    (define constructor (tuple-descriptor-constructor descriptor))
    (define predicate (tuple-descriptor-predicate descriptor))
    (define field-accessor
      (make-tuple-field-accessor descriptor field-position 'field))
    ...
    (define-match-expander pattern
      (syntax-parser
        [(_ field ...) #'(? predicate (app field-accessor field) ...)]))
    root-binding))

(module+ test
  (test-case (name-string define-tuple-type)
    (define-tuple-type point (x y))
    (check-pred point? (point 1 2))
    (check-equal? (point-x (point 1 2)) 1)
    (check-equal? (point-y (point 1 2)) 2)
    (check-equal? (point 1 2) (point 1 2))
    (check-pred point? (apply point (list 1 2)))
    (check-match (point 1 2) (point x _) (equal? x 1))
    (check-match (point 1 2) (point _ y) (equal? y 2))

    (test-case "should allow omitting the root binding"
      (define-tuple-type point (x y) #:omit-root-binding)
      (check-match (constructor:point 1 2) (pattern:point x y)
                   (and (equal? x 1) (equal? y 2)))
      (check-pred point? (apply constructor:point (list 1 2))))))
