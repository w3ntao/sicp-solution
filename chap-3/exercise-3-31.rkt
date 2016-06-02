#lang racket

(require (combine-in (only-in "../ToolBox/IntegratedCircuit/wire.rkt"
                              make-wire
                              add-action!
                              set-signal!
                              get-signal)
                     (only-in "../ToolBox/IntegratedCircuit/gate.rkt"
                              half-adder)
                     (only-in "../ToolBox/IntegratedCircuit/agenda.rkt"
                              make-agenda
                              current-time
                              first-agenda-item
                              remove-first-agenda-item!
                              empty-agenda?)))

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


(define the-agenda (make-agenda))

(define input-0 (make-wire))
(define input-1 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum the-agenda)
(probe 'carry carry the-agenda)

(half-adder input-0 input-1 sum carry the-agenda)

(set-signal! input-0 1)

(newline)
(propagate the-agenda)

(set-signal! input-1 1)

(newline)
(propagate the-agenda)