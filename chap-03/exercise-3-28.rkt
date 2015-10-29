#lang racket

(define (or-gate a-0 a-1 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a-0)
                                 (get-signal a-1))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output
                                  new-value)))))
  (add-action! a-0 or-action-procedure)
  (add-action! a-1 or-action-procedure))