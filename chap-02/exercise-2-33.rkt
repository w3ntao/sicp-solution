#lang racket

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (new-map p sequence)
  (accumulate (lambda (x y) (cons (p x ) y))
              nil
              sequence))

(define (new-append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (new-length sequence)
  (accumulate (lambda (x y) (+ y 1))
              0
              sequence))


(define test-case (list 1 2 3 4))

(define another-test-case (list 5 6 7 8 9))

(define (square x)
  (* x x))

(new-map square test-case)

(new-append (list 0) (new-append test-case another-test-case))

(append (list 0) test-case another-test-case)

(new-length (new-append (list 1) (new-append test-case another-test-case)))
;(test-it (car (car (list 0))))