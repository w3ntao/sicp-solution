#lang racket

(require (only-in (combine-in rnrs/base-6
                              rnrs/mutable-pairs-6)
                  cdr
                  list
                  set-cdr!))

(define nil '())

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x nil))

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

w

v