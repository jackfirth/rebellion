#lang racket/base


(provide
 (rename-out
  [triple-quoted-string-read read]
  [triple-quoted-string-read-syntax read-syntax]))


(require racket/match
         racket/string)


(define (triple-quoted-string-read in)
  (parameterize ([current-readtable (make-triple-quoted-string-readtable)])
    (read in)))
 

(define (triple-quoted-string-read-syntax src in)
  (parameterize ([current-readtable (make-triple-quoted-string-readtable)])
    (read-syntax src in)))
 

(define (make-triple-quoted-string-readtable)
  (make-readtable (current-readtable) #\" 'terminating-macro read-triple-string))


(define read-triple-string
  (case-lambda
    [(ch in) (read-triple-string-datum ch in)]
    [(ch in src line col pos) (read-triple-string-syntax ch in src line col pos)]))


(define (read-triple-string-datum first-char in)
  (define-values (_line col _pos) (port-next-location in))
  (define initial-col (sub1 col))
  (define result (regexp-match #rx"\"\"\n(.*)\"\"\"" in))
  (define contents (match result [(list _ contents) (bytes->string/utf-8 contents)]))
  (string->immutable-string
   (string-join
    (for/list ([line (in-lines (open-input-string contents))])
      (substring line initial-col))
    "\n")))
  

(define (read-triple-string-syntax first-char in src line col pos)
  (define result (regexp-match #rx"\"\"\n(.*)\"\"\"" in))
  (define contents (match result [(list _ contents) (bytes->string/utf-8 contents)]))
  (define span (+ (string-length contents) 6))
  (define dedented-contents
    (string->immutable-string
     (string-join
      (for/list ([line (in-lines (open-input-string contents))])
        (substring line col))
      "\n")))
  (datum->syntax #false dedented-contents (list src line col pos span)))
