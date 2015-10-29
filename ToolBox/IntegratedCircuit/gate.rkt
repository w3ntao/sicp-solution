#lang racket

(provide (all-defined-out))

(require (combine-in (only-in "agenda.rkt"
                              after-delay)
                     (only-in "wire.rkt"
                              nil
                              make-wire
                              set-signal!
                              get-signal
                              add-action!)))

;delay definition
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (logical-or a b)
  (if (and (= a 0)
           (= b 0))
      0
      1))

(define (logical-and a b)
  (if (and (= a 1)
           (= b 1))
      1
      0))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal -- LOGICAL-NOT" s))))


(define (inverter input output agenda)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value))
                   agenda)))
  (add-action! input invert-input)
  'OK)

(define (and-gate a-0 a-1 output agenda)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a-0)
                                  (get-signal a-1))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output
                                  new-value))
                   agenda)))
  (add-action! a-0 and-action-procedure)
  (add-action! a-1 and-action-procedure))

(define (or-gate a-0 a-1 output agenda)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a-0)
                                 (get-signal a-1))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output
                                  new-value))
                   agenda)))
  (add-action! a-0 or-action-procedure)
  (add-action! a-1 or-action-procedure))


(define (half-adder a b sum carry agenda)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d agenda)
    (and-gate a b carry agenda)
    (inverter carry e agenda)
    (and-gate d e sum agenda)
    'OK))

(define (full-adder a b c-in sum c-out agenda)
  (let ((s (make-wire))
        (c-0 (make-wire))
        (c-1 (make-wire)))
    (half-adder b c-in s c-0 agenda)
    (half-adder a s sum c-1 agenda)
    (or-gate c-0 c-1 c-out agenda)
    'OK))