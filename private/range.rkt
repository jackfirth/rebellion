#lang racket/base

(provide (rename-out [range/macro range])
         range?
         range/lower
         range/lower?
         range/lower-incl
         range/upper
         range/upper?
         range/upper-incl
         range/contains)

(require (for-syntax racket/base syntax/parse))
(begin-for-syntax
  (define-splicing-syntax-class value
    (pattern (~seq (~datum *))
             #:with v #'null)
    (pattern (~seq v:expr)))
  (define-splicing-syntax-class bound
    (pattern (~seq [v:expr i:boolean]))
    (pattern (~seq val:value)
             #:with v #'val.v
             #:with i #'null)))

(define-syntax (range/macro stx)
  (syntax-parse stx
    [(_ u:bound) #'(range/create 0 null u.v u.i)]
    [(_ l:bound u:bound) #'(range/create l.v l.i u.v u.i)]
    [(_ #:incl l:value u:value) #'(range/create l.v #t u.v #t)]
    [(_ #:excl l:value u:value) #'(range/create l.v #f u.v #f)]
    [(_ #:incl l:value #:excl u:value) #'(range/create l.v #t u.v #f)]
    [(_ #:excl l:value #:incl u:value) #'(range/create l.v #f u.v #t)]))

(struct range (lower upper))
(struct bound (value incl))
(define unbounded (bound null #f))

(define (range/create lower lower-incl upper upper-incl)
  (range (cond
           [(null? lower) unbounded]
           [(null? lower-incl) (bound lower #t)]
           [else (bound lower lower-incl)])
         (cond
           [(null? upper) unbounded]
           [(null? upper-incl) (bound upper #f)]
           [else (bound upper upper-incl)])))

(define (range/lower? range)
  (not (eq? (range-lower range) unbounded)))
(define (range/lower range)
  (bound-value (range-lower range)))
(define (range/lower-incl range)
  (bound-incl (range-lower range)))

(define (range/upper? range)
  (not (eq? (range-upper range) unbounded)))
(define (range/upper range)
  (bound-value (range-upper range)))
(define (range/upper-incl range)
  (bound-incl (range-upper range)))

(define (range/contains range value)
  (and (let ([lower (range-lower range)])
         (cond
           [(eq? lower unbounded) #t]
           [(bound-incl lower) (<= (bound-value lower) value)]
           [else (< (bound-value lower) value)]))
       (let ([upper (range-upper range)])
         (cond
           [(eq? upper unbounded) #t]
           [(bound-incl upper) (>= (bound-value upper) value)]
           [else (>= (bound-value upper) value)]))))
