#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  interleave
                  stream-map
                  integers
                  display-stream-until))

(define (pairs s t)
  (interleave (stream-map (lambda (x)
                            (list (stream-car s)
                                  x))
                          t)
              (pairs (stream-cdr s)
                     (stream-cdr t))))

(display-stream-until (pairs integers
                             integers)
                      20)