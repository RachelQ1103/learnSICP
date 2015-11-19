;---------test values---------------

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define  (edge1-frame f) (cadr f))
(define  (edge2-frame f) (caddr f))

;(origin-frame (make-frame (list 0 0) (list 1 0) (list 0 1)))
;(edge1-frame (make-frame (list 0 0) (list 1 0) (list 0 1)))
;(edge2-frame (make-frame (list 0 0) (list 1 0) (list 0 1)))