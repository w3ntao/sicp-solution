#lang racket

(provide (all-defined-out))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v-0 v-1)
  (make-vect (+ (xcor-vect v-0)
                (xcor-vect v-1))
             (+ (ycor-vect v-0)
                (ycor-vect v-1))))

(define (sub-vect v-0 v-1)
  (make-vect (- (xcor-vect v-0)
                (xcor-vect v-1))
             (- (ycor-vect v-0)
                (ycor-vect v-1))))


(define (scale-vect factor v)
  (make-vect (* factor
                (xcor-vect v))
             (* factor
                (ycor-vect v))))

(define (display-vect v)
  (display "(")
  (display (xcor-vect v))
  (display ",")
  (display (ycor-vect v))
  (display ")"))