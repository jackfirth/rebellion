#lang info

(define collection "rebellion")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list "Rebellion")
              "rebellion")))

(define deps
  (list "base"
        "lathe-comforts-lib"
        "reprovide-lang"))

(define build-deps
  (list "lathe-comforts-doc"
        "net-doc"
        "parendown-lib"
        "racket-doc"
        "rackunit-lib"
        "scribble-lib"))

(define test-omit-paths
  (list #rx"\\.scrbl$"))
