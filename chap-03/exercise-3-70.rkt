#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  stream-map
                  interleave
                  integers
                  display-stream-until))

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

;;; part a
(define (weight-a element)
  (+ (car element)
     (cadr element)))
;;; part b
(define (weight-b element)
  (let ((i (car element))
        (m (cadr element)))
    (+ (* 2 i)
       (* 3 m)
       (* 5 i m))))

(define (weighted-pairs s t weight)
  (cons-stream (list (stream-car s)
                     (stream-car t))
               (merge-weighted (stream-map (lambda (x)
                                             (list (stream-car s)
                                                   x))
                                           (stream-cdr t))
                               (weighted-pairs (stream-cdr s)
                                               (stream-cdr t)
                                               weight)
                               weight)))

(display-stream-until (weighted-pairs integers integers weight-a)
                      20)