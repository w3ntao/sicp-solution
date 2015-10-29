#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  add-streams
                  mul-streams
                  mul-series
                  invert-unit-series
                  scale-stream
                  stream-map
                  ones
                  integers
                  sine-series
                  cosine-series
                  stream-ref
                  display-stream-until))


(define (div-series s-0 s-1)
  (let ((c (stream-car s-1)))
    (if (= c 0)
        (error "constant term of s-1 can't be 0!")
        (scale-stream
         (mul-series s-0
                     (invert-unit-series (scale-stream s-1
                                                       (/ 1 c))))
         (/ 1 c)))))

(define tangent-series
  (div-series sine-series
              cosine-series))

(display-stream-until sine-series 10)

(newline)

(display-stream-until cosine-series 10)

(newline)

(display-stream-until tangent-series 10)