#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  cons-stream
                  stream-car
                  stream-cdr
                  stream-map
                  integers
                  display-stream-until))

(define (sign-change-detector this-value last-value)
  (cond ((and (< last-value 0)
              (or (= this-value 0)
                  (> this-value 0)))
         1)
        ((and (or (= last-value 0)
                  (> last-value 0))
              (< this-value 0))
         -1)
        (else
         0)))

(define (make-zero-crossings input-stream last-value last-avpt)
  (define (average a b)
    (/ (+ a b)
       2.0))
  (let ((avpt (average (stream-car input-stream)
                       last-value)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(define sense-data
  (stream-map (lambda (x)
                (if (or (= (remainder x 4)
                           0)
                        (= (remainder x 4)
                           1))
                    (* -1 x)
                    x))
              integers))

(display-stream-until (make-zero-crossings sense-data
                                           0
                                           0)
                      10)