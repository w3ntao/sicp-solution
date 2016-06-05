#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  stream-ref
                  stream-map
                  integers
                  stream-filter
                  merge-weighted
                  display-stream-until))

(define (square x)
  (* x x))

(define (sum-of-square a b)
  (+ (square a)
     (square b)))

(define (weight-sum-of-square element)
  (sum-of-square (car element)
                 (cadr element)))

(define (weighted-pairs-with-weight s t weight)
  (cons-stream (list (stream-car s)
                     (stream-car t)
                     (sum-of-square (stream-car s)
                                    (stream-car t)))
               (merge-weighted (stream-map (lambda (x)
                                             (list (stream-car s)
                                                   x
                                                   (sum-of-square (stream-car s)
                                                                  x)))
                                           (stream-cdr t))
                               (weighted-pairs-with-weight (stream-cdr s)
                                                           (stream-cdr t)
                                                           weight)
                               weight)))

(define (triple-duplicate-weight-proc stream)
  (define (organized-formula a b c)
    (list (list (car a)
                (cadr a))
          (list (car b)
                (cadr b))
          (list (car c)
                (cadr c))
          (caddr a)))
  (let ((first-element (stream-ref stream 0))
        (second-element (stream-ref stream 1))
        (third-element (stream-ref stream 2)))
    (let ((weight-a (caddr first-element))
          (weight-b (caddr second-element))
          (weight-c (caddr third-element)))
      (if (= weight-a
             weight-b
             weight-c)
          (cons-stream (organized-formula first-element
                                          second-element
                                          third-element)
                       (triple-duplicate-weight-proc (stream-cdr stream)))
          (triple-duplicate-weight-proc (stream-cdr stream))))))

(define triple-square-duplicate
  (triple-duplicate-weight-proc
   (weighted-pairs-with-weight integers
                               integers
                               weight-sum-of-square)))


(display-stream-until triple-square-duplicate 10)