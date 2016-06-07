#lang racket

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (add-0 a b)
  (if (= a 0)
      b
      (inc (add-0 (dec a)
              b))))

(define (add-1 a b)
  (if (= a 0)
      b
      (add-1 (dec a)
             (inc b))))

(add-0 4 5)
(+ 1 (add-0 3 5))
(+ 1 (+ 1 (add-0 2 5)))
(+ 1 (+ 1 (+ 1 (add-0 1 5))))
(+ 1 (+ 1 (+ 1 (+ 1 (add-0 0 5)))))
(+ 1 (+ 1 (+ 1 (+ 1 5))))
(+ 1 (+ 1 (+ 1 6)))
(+ 1 (+ 1 7))
(+ 1 8)
9

(newline)

(add-1 4 5)
(add-1 3 6)
(add-1 2 7)
(add-1 1 8)
(add-1 0 9)
9