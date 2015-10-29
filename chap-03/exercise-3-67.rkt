#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  stream-map
                  interleave
                  integers
                  display-stream-until))

(define (pairs s t)
  (cons-stream (list (stream-car s)
                     (stream-car t))
               (interleave (interleave (stream-map (lambda (x)
                                                     (list (stream-car s)
                                                           x))
                                                   (stream-cdr t))
                                       (stream-map (lambda (x)
                                                     (list x
                                                           (stream-car t)))
                                                   (stream-cdr s)))
                           (pairs (stream-cdr s)
                                  (stream-cdr t)))))

(display-stream-until (pairs integers integers)
                      30)