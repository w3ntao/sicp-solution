#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  add-streams
                  integers
                  display-stream-until))



(define (partial-streams stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-streams stream)
                           (stream-cdr stream))))

(display-stream-until (partial-streams integers)
                      10)