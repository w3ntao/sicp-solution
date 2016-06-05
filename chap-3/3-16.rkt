#lang racket

(require (only-in (combine-in rnrs/base-6
                              rnrs/mutable-pairs-6)
                  set-cdr!))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define test-0 (list 0 1 2 3))

(define (make-cycle x)
  (set-cdr! (last-pair x)
            x)
  x)

(define one (list 1))

(define two (cons one one))

(define seven (cons two two))

(define three (cons (cons 1 '()) (cons 2 '())))

(count-pairs three)