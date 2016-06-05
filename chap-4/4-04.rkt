#lang racket

(define (last-exp? x)
  (null? (cdr x)))

(define (eval-and exps)
  (cond ((last-exp? exps)
         (exps))
        ((true? (car exps))
         (eval-and (cdr exps)))
        (else
         #f)))

(define (eval-or exps)
  (cond ((last-exp? exps)
         (exps))
        ((true? (car exps))
         #t)
        (else
         (eval-or (cdr exps)))))
