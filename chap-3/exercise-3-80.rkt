#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  delay
                  add-streams
                  scale-stream
                  delayed-integral
                  display-stream-until))

(define (RLC R L C dt)
  (lambda (i-L-0 v-C-0)
    (define i-L
      (delayed-integral (delay d-i-L)
                        i-L-0
                        dt))
    (define v-C
      (delayed-integral (delay (scale-stream i-L
                                             (/ -1
                                                C)))
                        v-C-0
                        dt))
    (define d-i-L
      (add-streams (scale-stream v-C
                                 (/ 1
                                    L))
                   (scale-stream i-L
                                 (* (/ R
                                       L)
                                    -1))))
    (cons v-C i-L)))

(define temp-RLC
  ((RLC 1 1 0.2 0.1) 0
                     10))

(define length 10)

(display-stream-until (car temp-RLC)
                      length)
(newline)

(display-stream-until (cdr temp-RLC)
                      length)