#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  stream-map
                  stream-ref
                  display-stream-until))

(define (differ a b)
  (abs (- a b)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (in-circle circle-x circle-y r x-0 x-1 y-0 y-1)
  (define rand-x
    (if (< x-0 x-1)
        (random-in-range x-0 x-1)
        (random-in-range x-1 x-0)))
  (define rand-y
    (if (< y-0 y-1)
        (random-in-range y-0 y-1)
        (random-in-range y-1 y-0)))
  (< (+ (square (- rand-x circle-x))
        (square (- rand-y circle-y)))
     (square r)))

(define (square x)
  (* x x))

(define large-num 10000)
(define div-num 10000.0)

(define (random-in-range low high)
  (+ low
     (/ (random (* (- high low)
                   large-num))
        div-num)))

(define (in-circle-experiment-stream circle-x circle-y r x-0 x-1 y-0 y-1)
  (define (experiment)
    (in-circle circle-x circle-y
               r
               x-0 x-1
               y-0 y-1))
  (cons-stream experiment
               (in-circle-experiment-stream circle-x circle-y
                                             r
                                             x-0 x-1
                                             y-0 y-1)))

(define (monte-carlo-stream experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed
                    (+ passed failed))
                 (monte-carlo-stream (stream-cdr experiment-stream)
                                     passed
                                     failed)))
  (if ((stream-car experiment-stream))
      (next (+ passed 1)
            failed)
      (next passed
            (+ failed 1))))

(define (estimate-pi-stream circle-x circle-y r x-0 x-1 y-0 y-1)
  (let ((retangle-area (* (differ x-0 x-1)
                          (differ y-0 y-1))))
    (stream-map (lambda (p)
                  (/ (* p
                        retangle-area)
                     (square r)))
                (monte-carlo-stream
                 (in-circle-experiment-stream circle-x circle-y
                                               r
                                               x-0 x-1
                                               y-0 y-1)
                 0
                 0))))

(display-stream-until (estimate-pi-stream  1.0 1.0
                                           1.0
                                           0 3
                                           0 2)
                      1000)