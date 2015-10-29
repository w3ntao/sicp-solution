#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  add-streams
                  scale-stream
                  sine-series
                  cosine-series
                  display-stream-until))

(define (mul-series a-stream b-stream)
  (let ((a-0 (stream-car a-stream))
        (b-0 (stream-car b-stream)))
    (cons-stream (* a-0 b-0)
                 (add-streams (add-streams (scale-stream (stream-cdr b-stream)
                                                         a-0)
                                           (scale-stream (stream-cdr a-stream)
                                                         b-0))
                              (cons-stream 0
                                           (mul-series (stream-cdr a-stream)
                                                       (stream-cdr b-stream)))))))

(display-stream-until (mul-series sine-series
                                  sine-series)
                      10)
(newline)

(display-stream-until (mul-series cosine-series
                                  cosine-series)
                      10)

(newline)

(display-stream-until (add-streams (mul-series sine-series
                                               sine-series)
                                   (mul-series cosine-series
                                               cosine-series))
                      10)