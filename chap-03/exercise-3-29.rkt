#lang racket

(define (or-gate a b output)
  (let ((a-0 (make-wire))
        (b-0 (make-wire))
        (c (make-wire)))
    (inverter a a-0)
    (inverter b b-0)
    (and-gate a-0 b-0 c)
    (inverter c output)))

; or-gate delay = 2 * inverter-delay + and-gate-delay