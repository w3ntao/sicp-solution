#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  add-streams
                  display-stream-until))

(define s
  (cons-stream 1
               (add-streams s s)))

(display-stream-until s 10)