#lang racket/base

(module+ test
  (require rackunit
           rebellion/collection/list
           rebellion/private/static-name
           rebellion/streaming/transducer))

;@------------------------------------------------------------------------------

(module+ test
  (test-case (name-string transducer-impersonate)

    (define-values (impersonator-prop:secret-of-life
                  knows-secret-of-life?
                  secret-of-life)
    (make-impersonator-property 'secret-of-life))

    (define adding1 (mapping add1))
    
    (test-case "impersonator properties only"
      
      (define props (hash impersonator-prop:secret-of-life 42))
      (define impersonated (transducer-impersonate adding1 #:properties props))
      (check-equal? impersonated adding1)
      (check impersonator-of? impersonated adding1)
      (check impersonator-of? adding1 impersonated)
      (check chaperone-of? impersonated adding1)
      (check chaperone-of? adding1 impersonated)
      (check-pred knows-secret-of-life? impersonated)
      (check-equal? (secret-of-life impersonated) 42))

    (test-case "should enforce domain guard on input elements"
      (define seen (box empty-list))
      (define (guard v)
        (set-box! seen (list-append (unbox seen) (list v)))
        v)
      (define impersonated
        (transducer-impersonate adding1 #:domain-guard guard #:chaperone? #t))
      (define results (transduce (in-range 5) impersonated #:into into-list))
      (check-equal? results (list 1 2 3 4 5))
      (check-equal? (unbox seen) (list 0 1 2 3 4))
      (check-equal? impersonated adding1)
      (check impersonator-of? impersonated adding1)
      (check-false (impersonator-of? adding1 impersonated))
      (check chaperone-of? impersonated adding1)
      (check-false (chaperone-of? adding1 impersonated)))

    (test-case "should enforce range guard on output elements"
      (define seen (box empty-list))
      (define (guard v)
        (set-box! seen (list-append (unbox seen) (list v)))
        v)
      (define impersonated
        (transducer-impersonate adding1 #:range-guard guard #:chaperone? #t))
      (define results (transduce (in-range 5) impersonated #:into into-list))
      (check-equal? results (list 1 2 3 4 5))
      (check-equal? (unbox seen) (list 1 2 3 4 5))
      (check-equal? impersonated adding1)
      (check impersonator-of? impersonated adding1)
      (check-false (impersonator-of? adding1 impersonated))
      (check chaperone-of? impersonated adding1)
      (check-false (chaperone-of? adding1 impersonated)))))
