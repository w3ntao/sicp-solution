#lang racket

(define (make-let bindings body)
  (cons 'let
        (cons bindings
              body)))

(define (let*-bindings exp)
  (cadr exp))

(define (let*-body exp)
  (cddr exp))

(define (let*->nested-lets exp)
  (define (bindings-iter bindings)
    (if (null? (cdr bindings));;;if its the last element
        (make-let (list (car bindings)) 
                  (let*-body exp))
        (make-let (list (car bindings))
                  (bindings-iter (cdr bindings)))))
  (bindings-iter (let*-body exp)))