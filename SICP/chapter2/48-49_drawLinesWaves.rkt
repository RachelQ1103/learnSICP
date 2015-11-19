
#lang racket
(require ( planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;-------------48 make segments------------------
(define (make-segment2 v1 v2) (list v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cadr s))

;-----------49 drawings and wave procedures------------------------
(define outline-segments  
  (list   
    (make-segment (make-vect 0 0) (make-vect 0 1))  
      (make-segment (make-vect 0 1) (make-vect 1 1))  
      (make-segment (make-vect 1 1) (make-vect 1 0))  
      (make-segment (make-vect 1 0) (make-vect 0 0))))  
(define outline-painter (segments->painter outline-segments))  
  
(define diagonal-segments  
  (list   
    (make-segment (make-vect 0 0) (make-vect 1 1))  
      (make-segment (make-vect 0 1) (make-vect 1 0))))  
(define diagonal-painter (segments->painter diagonal-segments))  
  
(define diamonds-segments  
  (list   
    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))  
      (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))  
      (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))  
      (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5))))  
(define diamonds-painter (segments->painter diamonds-segments))

;-----------------------wave----------------------

(define wave (segments->painter (list
                         (make-segment (make-vect 0.4 1.0)      ; 头部左上
                                       (make-vect 0.35 0.85))
                         (make-segment (make-vect 0.35 0.85)    ; 头部左下
                                       (make-vect 0.4 0.64))
                         (make-segment (make-vect 0.4 0.65)     ; 左肩
                                       (make-vect 0.25 0.65))
                         (make-segment (make-vect 0.25 0.65)    ; 左手臂上部
                                       (make-vect 0.15 0.6))
                         (make-segment (make-vect 0.15 0.6)     ; 左手上部
                                       (make-vect 0.0 0.85))

                         (make-segment (make-vect 0.0 0.65)     ; 左手下部
                                       (make-vect 0.15 0.35))
                         (make-segment (make-vect 0.15 0.35)    ; 左手臂下部
                                       (make-vect 0.25 0.6))

                         (make-segment (make-vect 0.25 0.6)     ; 左边身体
                                       (make-vect 0.35 0.5))
                         (make-segment (make-vect 0.35 0.5)     ; 左腿外侧
                                       (make-vect 0.25 0.0))
                         (make-segment (make-vect 0.6 1.0)      ; 头部右上
                                       (make-vect 0.65 0.85))
                         (make-segment (make-vect 0.65 0.85)    ; 头部右下
                                       (make-vect 0.6 0.65))
                         (make-segment (make-vect 0.6 0.65)     ; 右肩
                                       (make-vect 0.75 0.65))
                         (make-segment (make-vect 0.75 0.65)    ; 右手上部
                                       (make-vect 1.0 0.3))

                         (make-segment (make-vect 1.0 0.15)     ; 右手下部
                                       (make-vect 0.6 0.5))
                         (make-segment (make-vect 0.6 0.5)      ; 右腿外侧
                                       (make-vect 0.75 0.0))

                         (make-segment (make-vect 0.4 0.0)      ; 左腿内侧
                                       (make-vect 0.5 0.3))
                         (make-segment (make-vect 0.6 0.0)      ; 右腿内侧
                                       (make-vect 0.5 0.3)))))


(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

;-----------new wave4----------------
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4-2 (flipped-pairs wave))

;-------test values-----------
;(paint wave)
;(paint wave2)
;(paint wave4)
;(paint wave4-2)
;(paint outline-painter)