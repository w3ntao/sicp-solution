#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  add-streams
                  delay
                  scale-stream
                  delayed-integral
                  stream-ref
                  ))

(define (solve-2nd a b dt y-0 dy-0)
  (define y
    (delayed-integral (delay dy)
                      y-0
                      dt))
  (define dy
    (delayed-integral (delay ddy)
                      dy-0
                      dt))
  (define ddy
    (add-streams (scale-stream dy
                               a)
                 (scale-stream y
                               b)))
  y)

(define large-number 10000)

(stream-ref (solve-2nd 1
                       0
                       (/ 1.0
                          large-number)
                       1
                       1)
            large-number)