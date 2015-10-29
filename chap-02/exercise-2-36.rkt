#lang racket

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op
                        init
                        (get-first-row seqs))
            (accumulate-n op init (get-remain-row seqs)))))

(define (get-first-row seq)
  (if (null? seq)
      nil
      (cons (car (car seq))
            (get-first-row (cdr seq)))))

(define (get-remain-row seq)
  (if (null? seq)
      nil
      (cons (cdr (car seq))
            (get-remain-row (cdr seq)))))

(define test-case (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;(get-first-row test-case)

;(get-remain-row test-case)

(accumulate-n + 0 test-case)

