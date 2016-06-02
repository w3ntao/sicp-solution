#lang racket

(define (let-parameters exp)
  (map car (cadr exp)))

(define (let-body exp)
  (caddr exp))

(define (let-expressions exp)
  (map cadr (cadr exp)))

(define (make-lambda parameters body)
  (cons 'lambda
        (cons parameters body)))

(define (let->combination exp)
  (cons (make-lambda (let-parameters exp)
                     (let-body exp))
        (let-expressions exp)))

;(let->combination (let ()))