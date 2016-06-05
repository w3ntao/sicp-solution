#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-map
                  integers
                  scale-stream
                  display-stream-until))

; part a
(define (integrate-series series)
  (stream-map /
              series
              integers))

; part b
(define cosine-series
  (cons-stream 1
               (scale-stream (integrate-series sine-series)
                             -1)))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

(display-stream-until sine-series 10)