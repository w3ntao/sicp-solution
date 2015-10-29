#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  scale-stream
                  merge
                  primes
                  display-stream-until))

(define S (cons-stream 1
                       (merge (merge (merge (scale-stream primes 2)
                                            (scale-stream primes 3))
                                     (merge (scale-stream primes 5)
                                            (scale-stream primes 6)))
                              (merge (merge (scale-stream primes 10)
                                            (scale-stream primes 15))
                                     (scale-stream primes 30)))))

(display-stream-until S 50)