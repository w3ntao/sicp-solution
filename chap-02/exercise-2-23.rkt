#lang racket

(define (for-each op raw-list)
  (when (not (null? raw-list))
    (op (car raw-list))
    (for-each op (cdr raw-list))))

(for-each (lambda (x) (display x) (newline))
          (list 1 2 3))

