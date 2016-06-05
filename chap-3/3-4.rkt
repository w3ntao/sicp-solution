#lang racket

(define (make-account balance password)
  (define try-limit 3)
  (define try-available try-limit)
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
  (define (call-the-cops)
    (display "police on the way")
    (newline))
  (define (dispatch psw cmd)
    (if (> try-available 0)
        (if (eq? psw password)
            (begin (set! try-available try-limit)
                   (cond ((eq? cmd 'withdraw) withdraw)
                         ((eq? cmd 'deposit) deposit)
                         (else (error "error try" cmd))))
            (lambda (x)
              (begin (set! try-available (- try-available 1))
                     "incorrect password")))
        (lambda (x)
          (call-the-cops))))
  dispatch)

(define acc (make-account 100 'pass))

((acc 'pass 'withdraw) 40)
((acc 'pass 'deposit) 40)

((acc 'passss 'withdraw) 500)
((acc 'pass 'withdraw) 500)
((acc 'passss 'withdraw) 500)

((acc 'passss 'withdraw) 500)
((acc 'passss 'withdraw) 500)
((acc 'passss 'withdraw) 500)