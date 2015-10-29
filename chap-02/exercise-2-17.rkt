#lang racket

(define squares (list 1 4 9 16 25 36))

(define odds (list 1 3 5 7 9 11))

(append squares odds)

(define (last-pair row-list)
  (if (null? (cdr row-list)) row-list
  (last-pair (cdr row-list))))

(last-pair odds)