#lang racket/base
(define (smallest x y z)
  (let ((a (if (< x y) x y)))
    (define b (if (< a z)a z))b)
  )
(define (square x)
  (* x x))
(define (sum-of-bigger-squares x y z)
  (- (+ (square x)
        (square y)
        (square z))
     (square (smallest x y z))))
  