#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (lambda (x) (f (f x))))

(define (repeat f n)
  (cond ((= n 1) (lambda (x) (f x)))
         ((even? n) (repeat (double f) (/ n 2)))
         (else (compose f (repeat f (- n 1))))))

((repeat square 2) 5)
