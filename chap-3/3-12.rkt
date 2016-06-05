#lang racket

(require (only-in (combine-in rnrs/base-6
                              rnrs/mutable-pairs-6)
                  cdr
                  set-cdr!
                  append
                  list))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x)
            y)
  x)

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

(cdr x)

(define w (append! x y))

(cdr x)