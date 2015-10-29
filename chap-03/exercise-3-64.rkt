#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  stream-map))

(define (steram-limit stream tolerance)
  (let ((item-0 (stream-car stream))
        (item-1 (stream-car (stream-cdr stream))))
    (if (< (difference item-0
                       item-1)
           tolerance)
        item-1
        (steram-limit (stream-cdr stream)
                      tolerance))))

(define (difference a b)
  (abs (- a b)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (sqrt-improve guess x)
  (average guess
           (/ x guess)))

(define (average x y)
  (/ (+ x y)
     2))

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

(steram-limit (sqrt-stream 100) 0.003)