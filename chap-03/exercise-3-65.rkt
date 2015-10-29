#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  add-streams
                  scale-stream
                  mul-streams
                  stream-map
                  integers
                  euler-transform
                  accelerated-sequence
                  display-stream-until))

(define interleave-ones
  (cons-stream 1
               (scale-stream interleave-ones
                             -1)))

(define stream-ln-2
  (cons-stream 0
               (add-streams (mul-streams interleave-ones
                                         (stream-map (lambda (x)
                                                       (/ 1 x))
                                                     integers))
                            stream-ln-2)))

(define num 4)

(display-stream-until stream-ln-2
                      num)
(newline)

(display-stream-until (euler-transform stream-ln-2)
                      num)

(newline)

(display-stream-until (accelerated-sequence euler-transform
                                            stream-ln-2)
                      num)