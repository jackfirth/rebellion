#lang racket/base

(require racket/contract/base)

(provide
 (rename-out [range/macro range])
 (contract-out
  [range/create (-> (or/c option? null?)
                    (or/c boolean? null?)
                    (or/c option? null?)
                    (or/c boolean? null?)
                    (or/c comparator? null?)
                    range?)]
  [range? predicate/c]
  [range/lower? (-> range? boolean?)]
  [range/lower (-> range? any/c)]
  [range/lower-incl? (-> range? boolean?)]
  [range/upper? (-> range? boolean?)]
  [range/upper (-> range? any/c)]
  [range/upper-incl? (-> range? boolean?)]
  [range/contains? (-> range? any/c boolean?)]))

(require (for-syntax racket/base
                     syntax/parse)
         rebellion/base/comparator
         rebellion/base/option)

(begin-for-syntax
  (define-splicing-syntax-class value
    (pattern (~seq (~datum *))
             #:with v #'absent)
    (pattern (~seq e:expr)
             #:with v #'(present e)))
  (define-splicing-syntax-class bound
    (pattern (~seq [e:expr i:boolean])
             #:with v #'(present e))
    (pattern (~seq val:value)
             #:with v #'val.v
             #:with i #'null))
  (define-splicing-syntax-class bounds
    #:attributes(l.v l.i u.v u.i)
    (pattern (~seq (~optional l:bound
                              #:defaults([l.v #'null] [l.i #'null]))
                   u:bound))
    (pattern (~seq (~or (~seq #:incl (~bind [l.i #'#t]))
                        (~seq #:excl (~bind [l.i #'#f])))
                   (~optional (~seq l:value
                                    (~or (~seq #:incl (~bind [u.i #'#t]))
                                         (~seq #:excl (~bind [u.i #'#f]))
                                         (~seq (~bind [u.i #'l.i]))))
                              #:defaults([l.v #'null] [u.i #'l.i]))
                   u:value))))

(define-syntax (range/macro stx)
  (syntax-parse stx
    [(_ b:bounds
        (~optional (~seq #:cmp c:expr)
                   #:defaults([c #'null])))
     #'(range/create b.l.v b.l.i b.u.v b.u.i c)]))

(struct range (lower-val lower-incl upper-val upper-incl comparator))

(define (range/create lower-val lower-incl upper-val upper-incl comparator)
  (range (if (null? lower-val) (present 0) lower-val)
         (if (null? lower-incl) #t lower-incl)
         upper-val
         (if (null? upper-incl) #f upper-incl)
         (if (null? comparator) real<=> comparator)))

(define (range/lower? range)
  (present? (range-lower-val range)))
(define (range/lower range)
  (present-value (range-lower-val range)))
(define (range/lower-incl? range)
  (and (range/lower? range) (range-lower-incl range)))

(define (range/upper? range)
  (present? (range-upper-val range)))
(define (range/upper range)
  (present-value (range-upper-val range)))
(define (range/upper-incl? range)
  (and (range/upper? range) (range-upper-incl range)))

(define (range/contains? range value)
  (define comparator (range-comparator range))
  (and (option-case (range-lower-val range)
         #:present (λ (lower)
                     (define cmp (compare comparator value lower))
                     (if (range-lower-incl range)
                         (not (eq? cmp lesser))
                         (eq? cmp greater)))
         #:absent (λ () #t))
       (option-case (range-upper-val range)
         #:present (λ (upper)
                     (define cmp (compare comparator value upper))
                     (if (range-upper-incl range)
                         (not (eq? cmp greater))
                         (eq? cmp lesser)))
         #:absent (λ () #t))))
