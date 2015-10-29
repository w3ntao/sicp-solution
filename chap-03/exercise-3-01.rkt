#lang racket


(define (make-accumulator val)
  (lambda (num)
    (begin (set! val (+ val num))
           val)))

(define A (make-accumulator 5))


(A 10)

(A 20)