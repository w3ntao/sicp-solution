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

(define (make-joint acc old-psw new-psw)
  (lambda (psw cmd)
    (if (eq? psw new-psw)
        (acc old-psw cmd)
        (error "wrong psw"))))

(define peter-acc (make-account 100 'old-one))

(define paul-acc (make-joint peter-acc 'old-one 'new-one))

((paul-acc 'new-one 'deposit) 50)

((peter-acc 'old-one 'withdraw) 20)