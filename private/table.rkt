#lang racket/base

(require racket/contract/base)

(provide
 columns
 row
 table
 (contract-out
  [table? (-> any/c boolean?)]
  [table-columns (-> table? (listof keyword?))]
  [table-columns-ref (-> table? keyword? list?)]
  [table-ref (-> table? natural? keyword? any/c)]
  [table-rows-ref (-> table? natural? record?)]
  [table-size (-> table? natural?)]))

(require (for-syntax racket/base)
         racket/list
         racket/math
         racket/pretty
         rebellion/generative-token
         rebellion/record
         syntax/parse/define)

(module+ test
  (require (submod "..")
           racket/format
           rackunit))

;@------------------------------------------------------------------------------

(define table-datatype-token (make-generative-token))

(struct table (record size)
  #:constructor-name plain-table
  #:omit-define-syntaxes

  #:methods gen:custom-write
  [(define (write-proc this out mode) (write-table this out mode))]

  #:methods gen:equal+hash
  [(define (equal-proc this other recur)
     (and (recur (table-size this) (table-size other))
          (recur (table-record this) (table-record other))))
   (define (hash-proc this recur)
     (recur (list table-datatype-token (table-record this))))
   (define hash2-proc hash-proc)])

(define-syntax (columns stx)
  (define msg "cannot be used outside a table expression")
  (raise-syntax-error 'columns msg stx))

(define-syntax (row stx)
  (define msg "cannot be used outside a table expression")
  (raise-syntax-error 'row msg stx))

(define-simple-macro
  (table ((~literal columns) column:keyword ...)
         (~and full-row ((~literal row) row-value:expr ...)) ...)
  #:do [(define column-keywords
          (sort (map syntax-e (syntax->list #'(column ...))) keyword<?))
        (define num-columns (length column-keywords))
        (define row-stxs (syntax->list #'(full-row ...)))
        (define num-rows (length row-stxs))]
  #:fail-when (findf (λ (row-stx)
                       (> (length (syntax->list row-stx))
                          (add1 num-columns)))
                     row-stxs)
  (format "too many values in row, table has only ~v columns" num-columns)
  #:fail-when (findf (λ (row-stx)
                       (< (length (syntax->list row-stx))
                          (add1 num-columns)))
                     row-stxs)
  (format "not enough values in row, table expects ~v columns" num-columns)
  #:with size num-rows
  #:with ((column-kw column-value ...) ...)
  (apply map
         list
         (syntax->list #'(column ...))
         (map syntax->list (syntax->list #'((row-value ...) ...))))
  #:with ((column-kw-arg ...) ...) #'((column-kw (list column-value ...)) ...)
  (plain-table (record column-kw-arg ... ...) 'size))

(define (table-columns-ref tab column)
  (record-ref (table-record tab) column))

(define (table-ref tab pos column)
  (list-ref (table-columns-ref tab column) pos))

(define (table-rows-ref tab pos)
  (record-map (table-record tab)
              (λ (column-values) (list-ref column-values pos))))

(define (table-columns tab)
  (record-keywords (table-record tab)))

(define (write-table tab out mode)
  (define default-custom-write
    (case mode
      [(#t) write]
      [(#f) display]
      [(0) (λ (v out) (print v out 0))]
      [(1) (λ (v out) (print v out 1))]))
  (define tab-rec (table-record tab))
  (define size (table-size tab))
  (define columns (record-keywords tab-rec))
  (define-values (ignored-out-line start-out-column ignored-out-position)
    (port-next-location out))
  (write-string "(table (columns" out)
  (for ([column (in-list columns)])
    (write-string " #:" out)
    (write-string (keyword->string column) out))
  (write-string ")" out)
  (let loop ([tab-rec tab-rec] [size size])
    (unless (zero? size)
      (write-char #\newline out)
      (write-string (make-string (+ (or start-out-column 0) 7) #\space) out)
      (write-string "(row" out)
      (for ([column (in-list columns)])
        (write-string " " out)
        (define v (first (record-ref tab-rec column)))
        (if (custom-write? v)
            ((custom-write-accessor v) v out mode)
            (default-custom-write v out)))
      (write-string ")" out)
      (loop (record-map tab-rec rest) (sub1 size))))
  (write-string ")" out)
  (void))

(module+ test
  (define countries
      (table (columns #:name #:population #:capital-city)
             (row "Argentina" 43800000 "Buenos Aires")
             (row "Greece" 10800000 "Athens")
             (row "Nigeria" 198600000 "Abuja")
             (row "Japan" 126400000 "Tokyo")))
  
  (test-case "table-ref"
    (check-equal? (table-ref countries 0 '#:name) "Argentina")
    (check-equal? (table-ref countries 2 '#:population) 198600000)
    (check-equal? (table-ref countries 3 '#:capital-city) "Tokyo"))

  (test-case "write-table"
    (check-equal? (~v countries)
                  #<<END
(table (columns #:capital-city #:name #:population)
       (row "Buenos Aires" "Argentina" 43800000)
       (row "Athens" "Greece" 10800000)
       (row "Abuja" "Nigeria" 198600000)
       (row "Tokyo" "Japan" 126400000))
END
                  )))
