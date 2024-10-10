#lang racket/base

(require racket/contract/base)

(provide
 columns
 row
 table
 for/table
 for*/table
 (contract-out
  [table? (-> any/c boolean?)]
  [table-columns (-> table? keyset?)]
  [table-columns-ref (-> table? keyword? immutable-vector?)]
  [table-ref (-> table? natural? keyword? any/c)]
  [table-rows-ref (-> table? natural? record?)]
  [table-size (-> table? natural?)]
  [in-table (-> table? (sequence/c record?))]
  [into-table (reducer/c record? table?)]))

(require (for-syntax racket/base)
         racket/math
         racket/sequence
         racket/stream
         rebellion/collection/immutable-vector
         rebellion/collection/keyset
         rebellion/collection/record
         guard
         rebellion/private/printer-markup
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/type/record
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define (in-table table)
  (for/stream ([row (in-range (table-size table))])
    (table-rows-ref table row)))

(define (make-table-properties descriptor)
  (define accessor (record-descriptor-accessor descriptor))
  (define fields (record-type-fields (record-descriptor-type descriptor)))
  (define backing-column-vectors-field
    (keyset-index-of fields '#:backing-column-vectors))
  (define size-field (keyset-index-of fields '#:size))
  (define custom-write
    (make-constructor-style-printer-with-markup
     (name table)
     (λ (this)
       (define vectors (accessor this backing-column-vectors-field))
       (define size (accessor this size-field))
       (define column-names (record-keywords vectors))
       (define columns-markup
         (sequence-markup
          (cons (unquoted-printing-string (name-string columns))
                (for/list ([k (in-keyset column-names)])
                  (unquoted-printing-string (string-append "#:" (keyword->string k)))))
          #:prefix (unquoted-printing-string "(")
          #:suffix (unquoted-printing-string ")")))
       (define rows-markup
         (for/list ([pos (in-range size)])
           (define row-values
             (for/list ([k (in-keyset column-names)])
               (table-ref this pos k)))
           (sequence-markup
            (cons (unquoted-printing-string (name-string columns)) row-values)
            #:prefix (unquoted-printing-string "(")
            #:suffix (unquoted-printing-string ")"))))
       (cons columns-markup rows-markup))))
  (list (cons prop:custom-write custom-write)
        (cons prop:sequence in-table)
        (cons prop:equal+hash (default-record-equal+hash descriptor))))

(define-record-type table (backing-column-vectors size)
  #:omit-root-binding
  #:property-maker make-table-properties)

(define-syntax (columns stx)
  (define msg "cannot be used outside a table expression")
  (raise-syntax-error 'columns msg stx))

(define-syntax (row stx)
  (define msg "cannot be used outside a table expression")
  (raise-syntax-error 'row msg stx))

(define-syntax-parse-rule (table ((~literal columns) column:keyword ...)
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
  #:with ((column-kw-arg ...) ...)
  #'((column-kw (immutable-vector column-value ...)) ...)
  (constructor:table #:backing-column-vectors (record column-kw-arg ... ...)
                     #:size 'size))

(define (table-columns-ref tab column)
  (record-ref (table-backing-column-vectors tab) column))

(define (table-ref tab pos column)
  (immutable-vector-ref (table-columns-ref tab column) pos))

(define (table-rows-ref tab pos)
  (record-map (table-backing-column-vectors tab)
              (λ (column-values) (immutable-vector-ref column-values pos))))

(define (table-columns tab)
  (record-keywords (table-backing-column-vectors tab)))

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
    (check-equal? (table-ref countries 3 '#:capital-city) "Tokyo")))

;@------------------------------------------------------------------------------

(define-record-type table-builder (size columns lists))

(define (empty-table-builder)
  (table-builder #:size 0
                 #:columns empty-keyset
                 #:lists (make-vector 0 #f)))

(define/guard (table-builder-add builder record)
  (define size (table-builder-size builder))
  (define columns (table-builder-columns builder))
  (define lists (table-builder-lists builder))
  (guard (positive? size) #:else
    (define lists
      (for/vector #:length (record-size record)
        ([v (in-vector (record-values record))])
        (list v)))
    (table-builder #:size 1
                   #:columns (record-keywords record)
                   #:lists lists))
  (unless (equal? columns (record-keywords record))
    (define msg "record contains different keys than previous records")
    (raise-arguments-error (name into-table)
                           msg
                           "record" record
                           "previous-keys" columns))
  (for ([lst (in-vector lists)]
        [i (in-naturals)])
    (define kw (keyset-ref columns i))
    (vector-set! lists i (cons (record-ref record kw) lst)))
  (table-builder #:size (add1 size)
                 #:columns columns
                 #:lists lists))

(define (build-table builder)
  (define size (table-builder-size builder))
  (define columns (table-builder-columns builder))
  (define lists (table-builder-lists builder))
  (define (build kw)
    (define mut-vec (make-vector size #f))
    (for ([v (in-list (vector-ref lists (keyset-index-of columns kw)))]
          [reverse-i (in-range (sub1 size) -1 -1)])
      (vector-set! mut-vec reverse-i v))
    (vector->immutable-vector mut-vec))
  (define columns-record (build-record build columns))
  (constructor:table #:backing-column-vectors columns-record #:size size))

(define/name into-table
  (make-effectful-fold-reducer table-builder-add
                               empty-table-builder
                               build-table
                               #:name enclosing-variable-name))

(define-syntaxes (for/table for*/table)
  (make-reducer-based-for-comprehensions #'into-table))

(module+ test
  (test-case "table-comprehensions"
    (define actual
      (for/table ([str (in-list (list "hello" "my" "good" "friend"))]
                  [i (in-naturals)])
        (record #:string str
                #:position i
                #:length (string-length str))))
    (check-equal? actual
                  (table (columns #:string #:position #:length)
                         (row "hello" 0 5)
                         (row "my" 1 2)
                         (row "good" 2 4)
                         (row "friend" 3 6)))))
