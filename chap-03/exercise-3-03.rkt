#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance
          (+ balance amount))
    balance)
  (define (dispatch psw cmd)
    (if (eq? psw password)
        (cond ((eq? cmd 'withdraw) withdraw)
              ((eq? cmd 'deposit) deposit)
              (else
               (error "error try" cmd)))
        (lambda (x)
          "incorrect password")))
  dispatch)

(define acc (make-account 100 'pass))

((acc 'pass 'withdraw) 40)

((acc 'passss 'deposit) 50)