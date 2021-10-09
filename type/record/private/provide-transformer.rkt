#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/provide-transform
                     rebellion/type/record/binding))

(module+ test
  (require rackunit
           syntax/location
           rebellion/private/static-name
           (submod "..")))

(provide record-out)

(define-syntax record-out
  (make-provide-transformer
   (λ (provide-spec modes)
     (syntax-parse provide-spec
       [(_ record:record-id)
        (expand-export
         #'(combine-out
            record
            record.descriptor
            record.predicate
            record.field-accessor ...)
         modes)]))))

(module+ test
  (module chair-module racket/base
    (require rebellion/type/record/private/definition-macro
             (submod ".." ".."))
    (provide (record-out chair))
    (define-record-type chair (legs seat screws)))
  (test-case (name-string record-out)
    (define mod (quote-module-path chair-module))
    (check-not-exn
     (λ ()
      (for ([sym '(#f chair chair? chair-legs chair-seat chair-screws)])
        (dynamic-require mod sym))))))
