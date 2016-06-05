#lang racket

(define (pascal y x)
  (when (< x (+ y 1))
    (if (or (= x 0)
            (= x y))
        1
        (+ (pascal (- y 1) (- x 1))
           (pascal (- y 1) x)))))

(define (display-pascal n)
  (define (space s)
    (when (not (= s 0))
      (display " ")
      (space (- s 1))))
  
  (define (display-num x)
    (when (< x 10)
      (space 1))
    (display x))
  
  (define (display-iter y x)
    (when (= x 0)
      (space (* 2 (- n y))))
    (display-num (pascal y x))
    (space 2)
    (if (< x y)
        (display-iter y (+ x 1))
        (when (< y n)
          (newline)
          (display-iter (+ y 1) 0))))
  
  (display-iter 0 0))

(display-pascal 7)