#lang racket

(provide (all-defined-out))

(require (only-in "vector.rkt"
                  make-vect
                  display-vect))

(define (make-segment v-0 v-1)
  (cons v-0 v-1))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (display-segment segment)
  (display-vect (start-segment segment))
  (display "-")
  (display-vect (end-segment segment))
  (newline))

#|
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
|#