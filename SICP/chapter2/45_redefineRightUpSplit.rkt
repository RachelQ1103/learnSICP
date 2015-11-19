#lang racket
(require ( planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;------------previous split operations-------------------
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

;--------------manipulate painter operations--------------

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;-----------2.55 redefine right & up-split----------------

(define (split proc1 proc2)
  (lambda (painter n)
    (if (= n 0) painter
        (let ((smaller ((split proc1 proc2) painter (- n 1))))
          (proc1 painter (proc2 smaller smaller))))))


(define right-split2 (split beside below))
(define up-split2 (split below beside))

;--------------test values-----------------
;(paint (flipped-pairs einstein))
;(paint (square-limit einstein 3))
;(paint (right-split2 einstein 3))
;(paint (up-split2 einstein 3))