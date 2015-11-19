#lang racket
(require ( planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;---------------------------Frames-----------------------
(define (frame-coord-map2 frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;----------------2.46 add/sub/scale vectors---------------
(define (make-vect2 x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect2 (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect2 (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect2 (* s (xcor-vect v))
             (* s (ycor-vect v))))


;---------------2.47 make frame ----------------------

(define (make-frame2 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

;-------------------------------------------------------

(define (transform-painter2 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
;---------------------------------------------------------
(define (flip-vert2 painter)
  (transform-painter2 painter
                     (make-vect2 0 1)
                     (make-vect2 1 1)
                     (make-vect2 0 0)))

(define (shrink-to-upper-right painter)
  (transform-painter2 painter
                     (make-vect2 0.5 0.5)
                     (make-vect2 1.0 0.5)
                     (make-vect2 0.5 1.0)))

(define (flip-horiz2 painter)
  (transform-painter2 painter
                     (make-vect2 1 0)
                     (make-vect2 0 0)
                     (make-vect2 1 1)))

;-------------------------------------------------

; rotate an image 45 degrees to the left
(define (rotate-45 painter)
  ((transform-painter (make-vect 0.5 0.0)
                      (make-vect 1.0 0.5)
                      (make-vect 0.0 0.5))
   painter))

; rotate an image 180 degrees to the left
(define (rotate-180 painter)
  ((transform-painter (make-vect 1 1)
                      (make-vect 0 1)
                      (make-vect 1 0))
   painter))

; rotate an image 270 degrees to the left
(define (rotate-270 painter)
  ((transform-painter (make-vect 0 1)
                      (make-vect 0 0)
                      (make-vect 1 1))
   painter))


;---------------test values---------------
;(paint (rotate-45 einstein))
;(paint (rotate-180 einstein))
;(paint (rotate-270 einstein))
;(paint (flip-horiz2 einstein))
;(paint (shrink-to-upper-right einstein))

; --------------wrong test: squash-inwards ----(wrong)
;(paint (squash-inwards einstein))
(define (squash-inwards painter)
  ((transform-painter (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65))
   painter))


