#lang racket

(require (combine-in (only-in "../ToolBox/IntegratedCircuit/wire.rkt"
                              make-wire
                              set-signal!
                              get-signal
                              add-action!)
                     (only-in "../ToolBox/IntegratedCircuit/gate.rkt"
                              full-adder)
                     (only-in "../ToolBox/AbstractionOfData/table.rkt"
                              make-table
                              insert!
                              lookup)
                     (only-in "../ToolBox/IntegratedCircuit/agenda.rkt"
                              make-agenda
                              current-time
                              first-agenda-item
                              empty-agenda?
                              remove-first-agenda-item!)))

; num stands for bit length
(define (ripple-carry-adder table-A table-B table-S num agenda)
  (define (A n)
    (lookup n table-A))
  (define (B n)
    (lookup n table-B))
  (define (S n)
    (lookup n table-S))
  (define (ripple-carry-adder-unit n C-in)
    (cond ((< n 0)
           C-in)
          (else
           (let ((C-out (make-wire)))
             (begin (full-adder (A n) (B n) C-in (S n) C-out agenda)

                    (ripple-carry-adder-unit (- n 1)
                                             C-out))))))

  (let ((C-0 (make-wire)))
    (set-signal! C-0 0)
    (ripple-carry-adder-unit (- num 1) C-0)))


(define (make-circuit n)
  (let ((table (make-table)))
    (define (make-circuit-iter num)
      (begin
        (insert! num (make-wire) table)
        (when (> num 0)
          (make-circuit-iter (- num 1)))))
    (begin
      (make-circuit-iter (- n 1))
      table)))


; display n bits circuit
(define (display-circuit circuit n)
  (define (display-circuit-unit num)
    (display (get-signal (lookup num circuit)))
    (display " ")
    (when (< num (- n 1))
      (display-circuit-unit (+ num 1))))
  (display-circuit-unit 0))


(define (propagate agenda)
  (if (empty-agenda? agenda)
      'done
      (let ((first-item (first-agenda-item agenda)))
        (first-item)
        (remove-first-agenda-item! agenda)
        (propagate agenda))))

(define (probe name wire agenda)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))


(define circuit-0 (make-circuit 2))
(display-circuit circuit-0 2)
(newline)

(define circuit-1 (make-circuit 2))
(display-circuit circuit-1 2)
(newline)

(define result (make-circuit 2))
(newline)

(define the-agenda (make-agenda))

(probe 'first-bit (lookup 0 result) the-agenda)
(probe 'second-bit (lookup 1 result) the-agenda)


(ripple-carry-adder circuit-0 circuit-1 result 2 the-agenda)

(set-signal! (lookup 0 circuit-0) 1)
(display-circuit circuit-0 2)
(newline)

(propagate the-agenda)

(set-signal! (lookup 1 circuit-0) 1)

(propagate the-agenda)

(newline)
(display-circuit result 1)
