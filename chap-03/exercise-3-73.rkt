#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  add-streams
                  scale-stream
                  integral
                  integers
                  duplicate-stream
                  display-stream-until))

(define ones
  (duplicate-stream 1))

(define (RC R C dt)
  (lambda (i-stream v-0)
    (add-streams (scale-stream i-stream
                               R)
                 (scale-stream (integral i-stream
                                         v-0
                                         dt)
                               (/ 1 C)))))

(display-stream-until ((RC 5 1 0.5) ones
                                    0)
                      10)
