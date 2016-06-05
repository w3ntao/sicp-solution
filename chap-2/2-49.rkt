#lang racket

(require (combine-in (only-in "../ToolBox/CoordinateSystem/vector.rkt"
                              make-vect
                              add-vect
                              scale-vect)
                     (only-in "../ToolBox/CoordinateSystem/frame.rkt"
                              make-frame
                              edge-1-frame
                              edge-2-frame)
                     (only-in "../ToolBox/CoordinateSystem/segment.rkt"
                              make-segment)))

(define test-frame (make-frame (make-vect 0 0)
                               (make-vect 2 0)
                               (make-vect 0 3)))

;part a
#|
(define (frame-edges-painter frame)
  (segments-painter (frame-edges frame) frame))
|#

(define (frame-edges frame)
  (define origin-point (make-vect 0 0))
  (let ((left-edge (make-segment origin-point (edge-2-frame frame)))
        (bottom-edge (make-segment origin-point (edge-1-frame frame)))
        (right-edge (make-segment (edge-2-frame frame)
                                  (add-vect (edge-2-frame frame) (edge-1-frame frame))))
        (top-edge (make-segment (edge-1-frame frame)
                                (add-vect (edge-1-frame frame) (edge-2-frame frame)))))
    (list left-edge right-edge bottom-edge top-edge)))


;(for-each display-segment (frame-edges test-frame))



;part b
#|
(define (frame-cross-painter frame)
  (segments-painter (frame-cross frame) frame))
|#

(define (frame-cross frame)
  (define origin-point (make-vect 0 0))
  (let ((tl-br (make-segment (edge-2-frame frame) (edge-1-frame frame)))
        (bl-tr (make-segment origin-point
                             (add-vect (edge-2-frame frame) (edge-1-frame frame)))))
    (list tl-br bl-tr)))

;(for-each display-segment (frame-cross test-frame))



;part c
#|
(define (frame-edges-middle-painter frame)
  (segments-painter (frame-edges-middle frame) frame))
|#
(define (middle-point v-0 v-1)
  (scale-vect 0.5 (add-vect v-0 v-1)))

(define (frame-edges-middle frame)
  (define origin-point (make-vect 0 0))
  (define left-middle (middle-point origin-point
                                    (add-vect origin-point (edge-2-frame frame))))
  (define bottom-middle (middle-point origin-point
                                      (add-vect origin-point (edge-1-frame frame))))
  (define right-middle (middle-point (add-vect origin-point (edge-1-frame frame))
                                     (add-vect origin-point (add-vect (edge-1-frame frame)
                                                                      (edge-2-frame frame)))))
  (define top-middle (middle-point (add-vect origin-point (edge-2-frame frame))
                                   (add-vect origin-point (add-vect (edge-1-frame frame) (edge-2-frame frame)))))
  (let ((left-top (make-segment left-middle top-middle))
        (top-right (make-segment top-middle right-middle))
        (right-bottom (make-segment right-middle bottom-middle))
        (bottom-left (make-segment bottom-middle left-middle)))
    (list left-top top-right right-bottom bottom-left)))

;(for-each display-segment (frame-edges-middle test-frame))