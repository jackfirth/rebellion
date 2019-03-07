#lang info

(define collection "rebellion")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list "Rebellion")
              "rebellion")))

(define deps
  (list "base"
        "reprovide-lang"))

(define build-deps
  (list "net-doc"
        "racket-doc"
        "rackunit-lib"
        "scribble-lib"))

(define test-omit-paths
  (list #rx"\\.scrbl$"))
