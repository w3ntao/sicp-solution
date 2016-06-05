#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  nil
                  cons-stream
                  stream-car
                  stream-cdr
                  delay
                  force
                  stream-map
                  stream-ref))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (integral (delay (stream-cdr integrand))
                           (+ (* (stream-car integrand)
                                 dt)
                              initial-value)
                           dt))))

(define (solve f y-0 dt)
  (define y
    (integral (delay dy)
              y-0
              dt))
  (define dy
    (stream-map f y))
  y)

(define large-number 10000)

(stream-ref (solve (lambda (y)
                     y)
                   1
                   (/ 1.0 large-number))
            large-number)