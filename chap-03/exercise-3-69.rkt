#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  nil
                  cons-stream
                  stream-car
                  stream-cdr
                  integers
                  pairs
                  stream-map
                  stream-filter
                  display-stream-until))

(define (in-stream? x stream)
  (cond ((= x
            (stream-car stream))
         #t)
        ((< x
            (stream-car stream))
         #f)
        (else
         (in-stream? x
                     (stream-cdr stream)))))

(define (square x) (* x x))

(define (sum-of-square a b)
  (+ (square a)
     (square b)))

(define (triples s t u)
  (stream-map (lambda (x)
                (append x
                        (list (sqrt (sum-of-square (car x)
                                                   (cadr x))))))
              (stream-filter (lambda (x)
                               (in-stream? (sum-of-square (car x)
                                                          (cadr x))
                                           (stream-map square
                                                       u)))
                             (pairs s t))))

(display-stream-until (triples integers
                               integers
                               integers)
                      5)