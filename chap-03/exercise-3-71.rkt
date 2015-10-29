#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  stream-ref
                  stream-map
                  interleave
                  integers
                  stream-filter
                  merge-weighted
                  display-stream-until))

(define (cube x)
  (* x x x))

(define (sum-of-cube a b)
  (+ (cube a)
     (cube b)))

(define (weight-sum-of-cube element)
  (sum-of-cube (car element)
               (cadr element)))

(define (weighted-pairs-with-weight s t weight)
  (cons-stream (list (stream-car s)
                     (stream-car t)
                     (sum-of-cube (stream-car s)
                                  (stream-car t)))
               (merge-weighted (stream-map (lambda (x)
                                             (list (stream-car s)
                                                   x
                                                   (sum-of-cube (stream-car s)
                                                                x)))
                                           (stream-cdr t))
                               (weighted-pairs-with-weight (stream-cdr s)
                                                           (stream-cdr t)
                                                           weight)
                               weight)))

(define (double-duplicate-weight-proc stream)
  (define (organized-formula a b)
    (list (list (car a)
                (cadr a))
          (list (car b)
                (cadr b))
          (caddr a)))
  (let ((first-element (stream-ref stream 0))
        (second-element (stream-ref stream 1)))
    (let ((weight-a (caddr first-element))
          (weight-b (caddr second-element)))
      (if (= weight-a weight-b)
          (cons-stream (organized-formula first-element
                                          second-element)
                       (double-duplicate-weight-proc (stream-cdr stream)))
          (double-duplicate-weight-proc (stream-cdr stream))))))

(define Ramanujan-num
  (double-duplicate-weight-proc
   (weighted-pairs-with-weight integers
                               integers
                               weight-sum-of-cube)))

(display-stream-until Ramanujan-num 10)