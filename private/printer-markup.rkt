#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [sequence-markup? predicate/c]
  [sequence-markup
   (->* ((sequence/c any/c))
        (#:indentation exact-nonnegative-integer?
         #:prefix any/c
         #:suffix any/c
         #:inline-separator any/c)
        sequence-markup?)]
  [inline-sequence-markup? predicate/c]
  [inline-sequence-markup
   (->* ((sequence/c any/c))
        (#:prefix any/c
         #:suffix any/c
         #:separator any/c)
        inline-sequence-markup?)]
  [mode-sensitive-markup? predicate/c]
  [mode-sensitive-markup
   (-> #:written-form any/c
       #:displayed-form any/c
       #:unquoted-printed-form any/c
       #:quoted-printed-form any/c
       mode-sensitive-markup?)]
  [make-constructor-style-printer-with-markup
   (-> symbol? (-> any/c (sequence/c any/c)) custom-write-function/c)]))


(require racket/pretty
         racket/sequence
         rebellion/custom-write
         guard)


;@----------------------------------------------------------------------------------------------------


(define default-element-indentation 2)


(struct sequence-markup (elements indentation prefix suffix inline-separator)
  #:omit-define-syntaxes
  #:constructor-name constructor:sequence-markup
  #:transparent
  #:guard
  (λ (elements indentation prefix suffix inline-separator _)
    (values (sequence->list elements) indentation prefix suffix inline-separator))
  
  #:methods gen:custom-write
  [(define/guard (write-proc this out mode)
     (define elements (sequence-markup-elements this))
     (define indentation (sequence-markup-indentation this))
     (define prefix (sequence-markup-prefix this))
     (define suffix (sequence-markup-suffix this))
     (define inline-separator (sequence-markup-inline-separator this))
     (define inline
       (inline-sequence-markup elements #:prefix prefix #:suffix suffix #:separator inline-separator))
     (guard (pretty-printing-with-finite-columns?) #:else
       (custom-write inline out mode))
     (unless (try-pretty-print-single-line inline out mode)
       (define multiline
         (multiline-sequence-markup
          elements #:indentation indentation #:prefix prefix #:suffix suffix))
       (custom-write multiline out mode)))])


(define (sequence-markup elements
                         #:indentation [indentation default-element-indentation]
                         #:prefix [prefix (unquoted-printing-string "")]
                         #:suffix [suffix (unquoted-printing-string "")]
                         #:inline-separator [inline-separator (unquoted-printing-string " ")])
  (constructor:sequence-markup elements indentation prefix suffix inline-separator))


(struct inline-sequence-markup (elements prefix suffix separator)
  #:omit-define-syntaxes
  #:constructor-name constructor:inline-sequence-markup
  #:transparent
  #:guard
  (λ (elements prefix suffix separator _) (values (sequence->list elements) prefix suffix separator))
  
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     
     (define (recur x)
       (custom-write x out mode #:recursive? #true))
     
     (define elements (inline-sequence-markup-elements this))
     (define last-element-index (sub1 (length elements)))
     (define prefix (inline-sequence-markup-prefix this))
     (define suffix (inline-sequence-markup-suffix this))
     (define separator (inline-sequence-markup-separator this))
     
     (for ([element (in-list elements)]
           [i (in-naturals)])
       (cond
         [(zero? i)
          (recur prefix)
          (recur element)]
         [(equal? i last-element-index)
          (recur separator)
          (recur element)
          (recur suffix)]
         [else
          (recur separator)
          (recur element)])))])


(define (inline-sequence-markup elements
                                #:prefix [prefix (unquoted-printing-string "")]
                                #:suffix [suffix (unquoted-printing-string "")]
                                #:separator [separator (unquoted-printing-string " ")])
  (constructor:inline-sequence-markup elements prefix suffix separator))


(struct multiline-sequence-markup (elements indentation prefix suffix)
  #:omit-define-syntaxes
  #:constructor-name constructor:multiline-sequence-markup
  #:transparent
  #:guard
  (λ (elements indentation prefix suffix _)
    (values (sequence->list elements) indentation prefix suffix))
  
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     
     (define (recur x)
       (custom-write x out mode #:recursive? #true))
     
     (define leading-indentation-amount
       (+ (port-next-column out) (multiline-sequence-markup-indentation this)))
     (define leading-spaces (make-string leading-indentation-amount #\space))
     (define elements (multiline-sequence-markup-elements this))
     (define last-element-index (sub1 (length elements)))
     (define prefix (multiline-sequence-markup-prefix this))
     (define suffix (multiline-sequence-markup-suffix this))
     
     (for ([element (in-list elements)]
           [i (in-naturals)])
       (cond
         [(zero? i)
          (recur prefix)
          (recur element)]
         [(equal? i last-element-index)
          (pretty-print-newline out (pretty-print-columns))
          (write-string leading-spaces out)
          (recur element)
          (recur suffix)]
         [else
          (pretty-print-newline out (pretty-print-columns))
          (write-string leading-spaces out)
          (recur element)])))])


(define (multiline-sequence-markup elements
                                   #:indentation [indentation default-element-indentation]
                                   #:prefix [prefix (unquoted-printing-string "")]
                                   #:suffix [suffix (unquoted-printing-string "")])
  (constructor:multiline-sequence-markup elements indentation prefix suffix))


(struct mode-sensitive-markup (written-form displayed-form unquoted-printed-form quoted-printed-form)
  #:omit-define-syntaxes
  #:constructor-name constructor:mode-sensitive-markup
  #:transparent
  
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (define chosen-form
       (case mode
         [(#true) (mode-sensitive-markup-written-form this)]
         [(#false) (mode-sensitive-markup-displayed-form this)]
         [(0) (mode-sensitive-markup-unquoted-printed-form this)]
         [(1) (mode-sensitive-markup-quoted-printed-form this)]))
     (custom-write chosen-form out mode #:recursive? #true))])


(define (mode-sensitive-markup #:written-form written-form
                               #:displayed-form displayed-form
                               #:unquoted-printed-form unquoted-printed-form
                               #:quoted-printed-form quoted-printed-form)
  (constructor:mode-sensitive-markup
   written-form displayed-form unquoted-printed-form quoted-printed-form))


(define (unreadable-object-markup printed-string written-string)
  (define printed-form (unquoted-printing-string printed-string))
  (define unreadable-form (unquoted-printing-string written-string))
  (mode-sensitive-markup
   #:written-form unreadable-form
   #:displayed-form unreadable-form
   #:unquoted-printed-form printed-form
   #:quoted-printed-form unreadable-form))


(define constructor-style-opening-delimiter
  (unreadable-object-markup "(" "#<"))


(define constructor-style-closing-delimiter
  (unreadable-object-markup ")" ">"))


(define constructor-style-type-name-suffix
  (unreadable-object-markup "" ":"))


(define (constructor-style-markup type-name contents)
  (define type-name-markup
    (inline-sequence-markup
     (list (unquoted-printing-string (symbol->string type-name)) constructor-style-type-name-suffix)
     #:separator (unquoted-printing-string "")))
  (sequence-markup
   (sequence-append (list type-name-markup) contents)
   #:prefix constructor-style-opening-delimiter
   #:suffix constructor-style-closing-delimiter))


(define ((make-constructor-style-printer-with-markup type-name get-contents) this out mode)
  (define markup (constructor-style-markup type-name (get-contents this)))
  (custom-write markup out mode))


(define (pretty-printing-with-finite-columns?)
  (and (pretty-printing) (integer? (pretty-print-columns))))


;; Any OutputPort PrintMode -> Boolean
;; Tries to print `v` to the output port on a single line. If `v` takes up more than one line, nothing
;; is printed to `out` and false is returned. If printing succeeds, true is returned.
(define (try-pretty-print-single-line v out mode)
  (let/ec escape
    
    (define (on-overflow)
      
      ;; We have to cancel the port before escaping instead of after escaping because the
      ;; tentative-port variable isn't in scope outside the let/ec expression.
      (tentative-pretty-print-port-cancel tentative-port)
      
      ;; We have to escape because make-tentative-pretty-print-output-port calls the overflow thunk
      ;; *each time* the content exceeds the column limit; it doesn't actually *stop* printing. The
      ;; only way to stop the print operation while inside the overflow thunk is to escape from the
      ;; thunk with a continuation jump.
      (escape #false))
    
    (define tentative-port
      (make-tentative-pretty-print-output-port out (pretty-print-columns) on-overflow))
    
    ;; If this exceeds the column width, the on-overflow thunk is called which aborts out using the
    ;; escape continuation.
    (custom-write v tentative-port mode)
    
    ;; If evaluation reaches this point, printing v did not exceed the column limit and we can commit
    ;; the tentative port's output, sending it to the original port.
    (tentative-pretty-print-port-transfer tentative-port out)
    
    #true))


(define (custom-write v out mode #:recursive? [recursive? #false])
  (if recursive?
      (case mode
        [(#t) (write v out)]
        [(#f) (display v out)]
        [(0 1) (print v out mode)])
      ((custom-write-accessor v) v out mode)))


(define (port-next-column out)
  (define-values (unused-line col unused-pos) (port-next-location out))
  col)


(module+ main
  (pretty-print (sequence-markup (list "foo" "bar" "baz")))
  (pretty-print
   (sequence-markup
    (list "foo" "bar" "baz")
    #:prefix (unquoted-printing-string "[")
    #:suffix (unquoted-printing-string "]")
    #:inline-separator (unquoted-printing-string ", ")))
  (pretty-print (constructor-style-markup 'list (list "foo" "bar" "baz")))
  (pretty-print
   (sequence-markup
    (list "fooooooooooooooooooooooooooooooooo"
          "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
          "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz")))
  (pretty-print
   (constructor-style-markup
    'multiset
    (list "fooooooooooooooooooooooooooooooooo"
          "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
          "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz"))))
