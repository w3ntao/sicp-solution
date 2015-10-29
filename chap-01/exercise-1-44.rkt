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


(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x)(/ (+ (f x)
                  (f (- x dx))
                  (f (+ x dx)))
               3))))

(define (smooth-n-time f n)
  (repeat (smooth f) n))

((repeat square 2) 5)
