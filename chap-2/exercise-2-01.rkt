#lang racket

(define (gcd a b)
  (if (= a 0) b
      (gcd (remainder b a) a)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< (/ d g) 0) (cons (- (/ n g)) (- (/ d g)))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-third (make-rat 1 -3))

(define four-sixth (make-rat -4 -6))

(print-rat one-third)

(print-rat four-sixth)

(print-rat (mul-rat one-third four-sixth))

