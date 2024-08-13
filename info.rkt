#lang info

(define collection "rebellion")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list "Data Structures")
              "rebellion")))

(define deps
  (list "base"
        "guard"))

(define build-deps
  (list "net-doc"
        "racket-doc"
        "rackunit-lib"
        "scribble-lib"))

(define test-omit-paths
  (list #rx"\\.scrbl$"))
