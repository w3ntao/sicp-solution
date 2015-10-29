#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-map
                  display-stream-until))

(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))

(define (mul-streams s-0 s-1)
  (stream-map * s-0 s-1))

(define factorials (cons-stream 1
                                (mul-streams (integers-starting-from 2)
                                             factorials)))

(display-stream-until factorials 5)