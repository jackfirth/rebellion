#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [endpoint-map-complement (-> sorted-map? sorted-map?)]
  [immutable-endpoint-map-complement (-> immutable-sorted-map? immutable-sorted-map?)]
  [mutable-endpoint-map-complement (-> mutable-sorted-map? mutable-sorted-map?)]))


(require racket/generic
         racket/stream
         rebellion/collection/entry
         rebellion/collection/private/sorted-map-interface
         rebellion/private/cut
         (submod rebellion/collection/private/sorted-map-interface private-for-rebellion-only))


;@----------------------------------------------------------------------------------------------------


(struct endpoint-map-complement abstract-sorted-map (original-endpoints)

  #:methods gen:sorted-map
  [(define/generic generic-sorted-map-key-comparator sorted-map-key-comparator)

   (define (this-endpoints this)
     (endpoint-map-complement-original-endpoints this))

   (define (in-sorted-map this #:descending? [descending? #false])
     (in-endpoint-map-complement (this-endpoints this) #:descending? descending?))

   (define (sorted-map-size this)
     (endpoint-map-complement-size (this-endpoints this)))

   (define (sorted-map-key-comparator this)
     (generic-sorted-map-key-comparator (this-endpoints this)))

   (define (sorted-map-contains-key? this key)
     (endpoint-map-complement-contains-key? (this-endpoints this) key))

   (define (sorted-map-contains-value? this key)
     (endpoint-map-complement-contains-value? (this-endpoints this) key))])


(struct immutable-endpoint-map-complement abstract-immutable-sorted-map (original-endpoints)

  #:methods gen:sorted-map

  [(define/generic generic-sorted-map-key-comparator sorted-map-key-comparator)

   (define (this-endpoints this)
     (immutable-endpoint-map-complement-original-endpoints this))

   (define (in-sorted-map this #:descending? [descending? #false])
     (in-endpoint-map-complement (this-endpoints this) #:descending? descending?))

   (define (sorted-map-size this)
     (endpoint-map-complement-size (this-endpoints this)))

   (define (sorted-map-key-comparator this)
     (generic-sorted-map-key-comparator (this-endpoints this)))

   (define (sorted-map-contains-key? this key)
     (endpoint-map-complement-contains-key? (this-endpoints this) key))

   (define (sorted-map-contains-value? this key)
     (endpoint-map-complement-contains-value? (this-endpoints this) key))]

  #:methods gen:immutable-sorted-map

  [(define (this-endpoints this)
     (immutable-endpoint-map-complement-original-endpoints this))])


(struct mutable-endpoint-map-complement abstract-mutable-sorted-map (original-endpoints)

  #:methods gen:sorted-map

  [(define/generic generic-sorted-map-key-comparator sorted-map-key-comparator)

   (define (this-endpoints this)
     (mutable-endpoint-map-complement-original-endpoints this))

   (define (in-sorted-map this #:descending? [descending? #false])
     (in-endpoint-map-complement (this-endpoints this) #:descending? descending?))

   (define (sorted-map-size this)
     (endpoint-map-complement-size (this-endpoints this)))

   (define (sorted-map-key-comparator this)
     (generic-sorted-map-key-comparator (this-endpoints this)))

   (define (sorted-map-contains-key? this key)
     (endpoint-map-complement-contains-key? (this-endpoints this) key))

   (define (sorted-map-contains-value? this key)
     (endpoint-map-complement-contains-value? (this-endpoints this) key))]
  
  #:methods gen:mutable-sorted-map

  [(define (this-endpoints this)
     (mutable-endpoint-map-complement-original-endpoints this))])


(define (in-endpoint-map-complement original-endpoints #:descending? descending?)
  (if descending?
      (in-endpoint-map-complement-descending original-endpoints)
      (in-endpoint-map-complement-ascending original-endpoints)))


(define (in-endpoint-map-complement-ascending original-endpoints)
  (for/stream ([previous-entry (stream-cons #false (in-sorted-map original-endpoints))]
               [next-entry (stream-append (in-sorted-map original-endpoints) (stream #false))]
               #:unless (and (not previous-entry)
                             next-entry
                             (equal? (entry-key next-entry) bottom-cut))
               #:unless (and previous-entry
                             (not next-entry)
                             (equal? (entry-value previous-entry) top-cut)))
    (define lower (if previous-entry (entry-value previous-entry) bottom-cut))
    (define upper (if next-entry (entry-key next-entry) top-cut))
    (entry lower upper)))


(define (in-endpoint-map-complement-descending original-endpoints)
  (for/stream ([previous-entry
                (stream-cons #false (in-sorted-map original-endpoints #:descending? #true))]
               [next-entry
                (stream-append (in-sorted-map original-endpoints #:descending? #true)
                               (stream #false))]
               #:unless (and (not previous-entry)
                             next-entry
                             (equal? (entry-value next-entry) top-cut))
               #:unless (and previous-entry
                             (not next-entry)
                             (equal? (entry-key previous-entry) bottom-cut)))
    (define lower (if next-entry (entry-value next-entry) bottom-cut))
    (define upper (if previous-entry (entry-key previous-entry) top-cut))
    (entry lower upper)))


(define (endpoint-map-complement-size original-endpoints)
  (+ (sorted-map-size original-endpoints)
     1
     (if (equal? (sorted-map-least-key original-endpoints) bottom-cut) -1 0)
     (if (equal? (entry-value (sorted-map-greatest-entry original-endpoints)) top-cut) -1 0)))


(define (endpoint-map-complement-contains-key? original-endpoints key)
  (or (sorted-map-contains-value? original-endpoints key)
      (and (equal? key bottom-cut)
           (not (equal? (sorted-map-least-key original-endpoints) bottom-cut)))))


(define (endpoint-map-complement-contains-value? original-endpoints value)
  (or (sorted-map-contains-key? original-endpoints value)
      (and (equal? value top-cut)
           (not (equal? (entry-value (sorted-map-greatest-entry original-endpoints)) top-cut)))))
