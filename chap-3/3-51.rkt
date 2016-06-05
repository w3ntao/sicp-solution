#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  stream-enumerate-interval
                  stream-map
                  stream-ref))

(define (show x)
  (display x)
  (newline)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)

(stream-ref x 7)