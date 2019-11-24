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

(require (for-syntax racket/base
                     syntax/parse)
         rebellion/base/comparator)

(begin-for-syntax
  (define-splicing-syntax-class value
    (pattern (~seq (~datum *))
             #:with v #'null)
    (pattern (~seq v:expr)))
  (define-splicing-syntax-class bound
    (pattern (~seq [v:expr i:boolean]))
    (pattern (~seq val:value)
             #:with v #'val.v
             #:with i #'null))
  (define-splicing-syntax-class bounds
    #:attributes(l.v l.i u.v u.i)
    (pattern (~seq (~optional l:bound
                              #:defaults([l.v #'0] [l.i #'null]))
                   u:bound))
    (pattern (~seq (~or (~seq #:incl (~bind [l.i #'#t]))
                        (~seq #:excl (~bind [l.i #'#f])))
                   (~optional (~seq l:value
                                    (~or (~seq #:incl (~bind [u.i #'#t]))
                                         (~seq #:excl (~bind [u.i #'#f]))
                                         (~seq (~bind [u.i #'l.i]))))
                              #:defaults([l.v #'0] [u.i #'l.i]))
                   u:value))))

(define-syntax (range/macro stx)
  (syntax-parse stx
    [(_ b:bounds
        (~optional (~seq #:cmp c:expr)
                   #:defaults([c #'null])))
     #'(range/create b.l.v b.l.i b.u.v b.u.i c)]))

(struct range (lower upper comparator))
(struct bound (value incl))
(define unbounded (bound null #f))

(define (range/create lower lower-incl upper upper-incl comparator)
  (range (cond
           [(null? lower) unbounded]
           [(null? lower-incl) (bound lower #t)]
           [else (bound lower lower-incl)])
         (cond
           [(null? upper) unbounded]
           [(null? upper-incl) (bound upper #f)]
           [else (bound upper upper-incl)])
         (if (null? comparator) real<=> comparator)))

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
  (define comparator (range-comparator range))
  (and (let ([lower (range-lower range)])
         (or (eq? lower unbounded)
             (let ([cmp (compare comparator (bound-value lower) value)])
               (if (bound-incl lower)
                   (not (eq? cmp greater))
                   (eq? cmp lesser)))))
       (let ([upper (range-upper range)])
         (or (eq? upper unbounded)
             (let ([cmp (compare comparator (bound-value upper) value)])
               (if (bound-incl upper)
                   (not (eq? cmp lesser))
                   (eq? cmp greater)))))))
