#lang racket

(define random-init 79)

(define (rand-update x)
  (remainder (* x 973)
             37))

(define rand
  (let ((x random-init))
    (define (dispatch command)
      (cond ((eq? command 'generate)
             (set! x
                   (rand-update x))
             x)
            ((eq? command 'reset)
             (lambda (new-value)
               (set! x
                     new-value)
               x))))
    dispatch))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

(newline)

((rand 'reset) 13)
(rand 'generate)
(rand 'generate)