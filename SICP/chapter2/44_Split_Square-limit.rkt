#lang racket
(require ( planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;---------2.44 right &up &corner-split &square-limit--------

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0) painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0) painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (corner (corner-split painter (- n 1)))
              (bottom-right (below right right)))
         (beside (below painter top-left) (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((corner (corner-split painter n)))
    (let ((half (beside (flip-horiz corner) corner)))
          (below (flip-vert half) half))))

;--------------test values------------
;(paint (right-split einstein 3))
;(paint (up-split einstein 3))
;(paint (corner-split einstein 3))
;(paint (square-limit einstein 3))