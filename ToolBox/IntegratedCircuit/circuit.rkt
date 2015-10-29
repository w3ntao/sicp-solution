#lang racket

(require (combine-in (only-in "../AbstractionOfData/table.rkt"
                              make-table
                              insert!
                              lookup)
                     (only-in "wire.rkt"
                              make-wire
                              set-signal!
                              get-signal
                              add-action!)
                     (only-in "gate.rkt"
                              full-adder)
                     (only-in "agenda.rkt"
                              make-agenda
                              current-time
                              first-agenda-item
                              empty-agenda?
                              remove-first-agenda-item!)))

(define (ripple-carry-adder circuit-A circuit-B circuit-S agenda)
  (define (A n)
    (get-wire n circuit-A))
  (define (B n)
    (get-wire n circuit-B))
  (define (S n)
    (get-wire n circuit-S))
  (define (ripple-carry-adder-unit n C-in)
    (cond ((< n 0)
           C-in)
          (else
           (let ((C-out (make-wire)))
             (begin (full-adder (A n)
                                (B n)
                                C-in
                                (S n)
                                C-out
                                agenda)
                    (ripple-carry-adder-unit (- n 1)
                                             C-out))))))
  
  (define (same-number? a b c)
    (and (= a b)
         (= a c)
         (= b c)))
  
  (let ((C-0 (make-wire)))
    (set-signal! C-0 0)
    (cond ((same-number? (bit-length circuit-A)
                         (bit-length circuit-B)
                         (bit-length circuit-S))
           (ripple-carry-adder-unit (- (bit-length circuit-S)
                                       1)
                                    C-0))
          (else
           (error "bit length mismatched -- RIPPLE-CARRY-ADDER")))))


; build n bits circuit
(define (make-circuit num)
  (let ((circuit (make-table)))
    (define (legal-range? n)
      (and (< n num)
           (> n -1)))
    (define (put-wire n)
      (begin
        (insert! n
                 (make-wire)
                 circuit)
        (when (> n 0)
          (put-wire (- n 1)))))
    (define (set-my-bit! n new-value)
      (cond ((legal-range? n)
             (set-signal! (lookup n circuit)
                          new-value))
            (else
             (error "Invalid bit -- SET-MY-BITS!" n))))
    (define (get-my-bit n)
      (cond ((legal-range? n)
             (get-signal (lookup n
                                 circuit)))
            (else
             (error "Invalid bit -- GET-MY-BIT" n))))
    (define (get-my-wire n)
      (cond ((legal-range? n)
             (lookup n
                     circuit))
            (else
             (error "Invalid wire -- GET-MY-WIRE" n))))
    (define (display-my-circuit)
      (define (display-circuit-unit n)
        (display (get-my-bit n))
        (display " ")
        (when (< n
                 (- num 1))
          (display-circuit-unit (+ n 1))))
      (display-circuit-unit 0)
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'get-bit) get-my-bit)
            ((eq? m 'set-bit!) set-my-bit!)
            ((eq? m 'get-wire) get-my-wire)
            ((eq? m 'display-circuit) display-my-circuit)
            ((eq? m 'bit-length) num)
            (else
             (error "Unknown operation -- CIRCUIT" m))))
    
    (cond ((> num 0)
           (begin
             (put-wire (- num 1))
             dispatch))
          (else
           (error "Invalid bit -- MAKE-CIRCUIT" num)))))

(define (get-bit n circuit)
  ((circuit 'get-bit) n))

(define (set-bit! n new-value circuit)
  ((circuit 'set-bit!) n new-value))

(define (get-wire n circuit)
  ((circuit 'get-wire) n))

(define (display-circuit circuit)
  ((circuit 'display-circuit)))

(define (bit-length circuit)
  (circuit 'bit-length))

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

(define (circuit-probe name circuit agenda)
  (define (wire-probe name wire)
    (add-action! wire
                 (lambda ()
                   (display name)
                   (display " ")
                   (display (current-time agenda))
                   (newline)
                   (display-circuit circuit))))
  (define (circuit-probe-unit n)
    (wire-probe name
                (get-wire n circuit))
    (when (< n
             (- (bit-length circuit)
                1))
      (circuit-probe-unit (+ n 1))))
  (circuit-probe-unit 0))


(define the-agenda (make-agenda))

(define circuit-a (make-circuit 3))

(define circuit-b (make-circuit 3))

(define circuit-s (make-circuit 3))



(circuit-probe 'circuit-s circuit-s the-agenda)


(ripple-carry-adder circuit-a circuit-b circuit-s the-agenda)


(set-bit! 0 1 circuit-a)
(set-bit! 1 1 circuit-a)


;(set-bit! 1 1 circuit-b)
(set-bit! 2 1 circuit-b)

(propagate the-agenda)

(newline)
(display "circuit s")
(newline)
(display-circuit circuit-s)
(newline)
