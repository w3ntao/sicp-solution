#lang racket

(require (only-in (combine-in rnrs/base-6
                              rnrs/mutable-pairs-6)
                  car
                  cdr
                  list
                  set-cdr!))

(define nil '())

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x)
            x)
  x)

(define (cycle-check raw-list)
  (define (cycle-check-iter r-list head)
    (cond ((null? r-list)
           #f)
          ((eq? (car r-list)
                head)
           #t)
          (else
           (cycle-check-iter (cdr r-list)
                             head))))
  (cycle-check-iter (cdr raw-list)
                    (car raw-list)))

(define test-0 (list 0 1 2 3))

(define test-1 (list 0 0 0 0))

(cycle-check test-0)

(cycle-check (make-cycle test-0))

(cycle-check test-1)

(cycle-check (make-cycle test-1))