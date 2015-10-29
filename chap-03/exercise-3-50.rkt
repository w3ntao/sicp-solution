#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  nil
                  cons-stream
                  stream-car
                  stream-cdr
                  integers
                  display-stream-until))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      nil
      (cons-stream
       (apply proc
              (map stream-car
                   argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(display-stream-until (stream-map + integers integers)
                      20)