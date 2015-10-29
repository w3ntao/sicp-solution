#lang racket

(provide (all-defined-out))

(require (only-in "../Math/prime.rkt"
                  prime?
                  square))

(define nil '())

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result
                       (proc))
                 (set! already-run?
                       true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((_ exp)
     (memo-proc (lambda () exp)))))

(define (force delayed-obj)
  (delayed-obj))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ head tail)
     (cons head
           (delay tail)))))
#|
(define (cons-stream a b)
  (cons a
        (delay b)))
|#
(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream)
                  (- n 1))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      nil
      (cons-stream
       (apply proc
              (map stream-car
                   argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (stream-for-each proc stream)
  (if (null? stream)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc
                              (stream-cdr stream)))))

(define (stream-filter pred stream)
  (cond ((null? stream)
         nil)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
         (stream-filter pred
                        (stream-cdr stream)))))

(define (merge s-0 s-1)
  (cond ((null? s-0) s-1)
        ((null? s-1) s-0)
        (else
         (let ((s-0-car (stream-car s-0))
               (s-1-car (stream-car s-1)))
           (cond ((< s-0-car s-1-car)
                  (cons-stream s-0-car
                               (merge (stream-cdr s-0)
                                      s-1)))
                 ((< s-1-car s-0-car)
                  (cons-stream s-1-car
                               (merge s-0
                                      (stream-cdr s-1))))
                 (else
                  (cons-stream s-0-car
                               (merge (stream-cdr s-0)
                                      (stream-cdr s-1)))))))))

(define (add-streams s-0 s-1)
  (stream-map + s-0 s-1))

(define (mul-streams s-0 s-1)
  (stream-map * s-0 s-1))

(define (scale-stream stream factor)
  (stream-map (lambda (x)
                (* x factor))
              stream))

(define (display-line x)
  (display x)
  (newline))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream-until stream n)
  (when (> n 0)
    (display-line (stream-car stream)))
  (when (> n 1)
    (display-stream-until (stream-cdr stream)
                          (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      nil
      (cons-stream low
                   (stream-enumerate-interval (+ low 1)
                                              high))))

(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define primes
  (cons-stream 2
               (stream-filter prime? (integers-starting-from 3))))

(define ones
  (cons-stream 1
               ones))

(define (integrate-series stream)
              (stream-map *
                          (stream-map /
                                      ones
                                      integers)
                          stream))

(define (duplicate-stream item)
  (cons-stream item
               (duplicate-stream item)))

(define (mul-series a-stream b-stream)
  (let ((a-0 (stream-car a-stream))
        (b-0 (stream-car b-stream)))
    (cons-stream (* a-0 b-0)
                 (add-streams (add-streams (scale-stream (stream-cdr b-stream)
                                                         a-0)
                                           (scale-stream (stream-cdr a-stream)
                                                         b-0))
                              (cons-stream 0
                                           (mul-series (stream-cdr a-stream)
                                                       (stream-cdr b-stream)))))))

(define (invert-unit-series series)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr series)
                                         (invert-unit-series series))
                             -1)))

(define exp-series
  (cons-stream 1
               (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
               (scale-stream (integrate-series sine-series)
                             -1)))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

(define (euler-transform s)
  (let ((s-0 (stream-ref s 0))
        (s-1 (stream-ref s 1))
        (s-2 (stream-ref s 2)))
    (cons-stream (- s-2
                    (/ (square (- s-2 s-1))
                       (+ s-0
                          (* -2 s-1)
                          s-2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (interleave s-0 s-1)
  (if (null? s-0)
      s-1
      (cons-stream (stream-car s-0)
                   (interleave s-1
                               (stream-cdr s-0)))))

(define (pairs s t)
  (cons-stream (list (stream-car s)
                     (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s)
                                                         x))
                                       (stream-cdr t))
                           (pairs (stream-cdr s)
                                  (stream-cdr t)))))

(define (merge-weighted s t weight)
  (cond ((null? s) t)
        ((null? t) s)
        (else
         (let ((s-car (stream-car s))
               (t-car (stream-car t)))
           (cond ((< (weight s-car)
                     (weight t-car))
                  (cons-stream s-car
                               (merge-weighted (stream-cdr s)
                                               t
                                               weight)))
                 (else
                  (cons-stream t-car
                               (merge-weighted s
                                               (stream-cdr t)
                                               weight))))))))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand
                                            dt)
                              int)))
  int)

(define (delayed-integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)