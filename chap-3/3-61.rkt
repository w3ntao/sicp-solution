#lang racket

(provide (all-defined-out))

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-cdr
                  scale-stream
                  mul-series
                  display-stream-until
                  exp-series))

(define (invert-unit-series series)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr series)
                                         (invert-unit-series series))
                             -1)))

(display-stream-until exp-series
                      10)

(newline)

(display-stream-until (invert-unit-series exp-series)
                      10)

(newline)

(display-stream-until (mul-series exp-series
                                  (invert-unit-series exp-series))
                      10)