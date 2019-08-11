#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [spliced-printing-entry (-> any/c any/c spliced-printing-entry?)]
  [spliced-printing-entry? any/c]))

(module+ test
  (require (submod "..")
           racket/pretty
           rackunit))

;@------------------------------------------------------------------------------

(struct spliced-printing-entry (key value)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (define (recur x)
       (case mode
         ((#t) (write x out))
         ((#f) (display x out))
         ((0 1) (print x out mode))))
     (recur (spliced-printing-entry-key this))
     (write-string " " out)
     (recur (spliced-printing-entry-value this)))])

(module+ test
  (test-case "spliced-printing-entry"
    (define v
      (list (spliced-printing-entry 'foo 1)
            (spliced-printing-entry 'bar 2)
            (spliced-printing-entry 'baz 3)))
    (check-equal? (pretty-format v 10) "'(foo 1\n  bar 2\n  baz 3)")))
