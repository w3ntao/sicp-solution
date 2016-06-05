#lang racket

(define (make-monitored f)
  (define count 0)
  (define (dispatch command)
    (cond ((eq? command 'reset)
           (begin (set! count 0)
                  count))
          ((eq? command 'how-many)
           count)
          (else
           (begin (set! count (+ count 1))
                  (f command)))))
  dispatch)

(define s (make-monitored sqrt))

(s 100)
(s 100)
(display (s 'how-many))