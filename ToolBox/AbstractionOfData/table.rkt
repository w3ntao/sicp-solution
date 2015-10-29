#lang racket

(provide (all-defined-out))

(require (only-in (combine-in rnrs/base-6
                              rnrs/mutable-pairs-6)
                  cons
                  car
                  caar
                  cdr
                  set-car!
                  set-cdr!
                  list))

(define nil '())

(define (make-table)
  (list '*table*))

(define (associate key records)
  (cond ((null? records)
         false)
        ((equal? key (caar records))
         (car records))
        (else
         (associate key (cdr records)))))

(define (insert! key value table)
  (let ((record (associate key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'OK)

(define (lookup key table)
  (let ((record (associate key (cdr table))))
    (if record
        (cdr record)
        false)))