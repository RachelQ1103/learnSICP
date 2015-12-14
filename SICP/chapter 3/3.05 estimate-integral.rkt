#lang racket/base

(define (square x) (* x x))
;----------random-in-range----------

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;------------monte-carlo test----------

(define (monte-carlo trails test)
  (define (iter trails-remaining trials-passed)
    (cond ((= trails-remaining 0) (/ trials-passed trails))
          ((test)
           (iter (- trails-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trails-remaining 1) trials-passed))))
  (iter trails 0))

;-----------------3.05 estimate-integral-----------------

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))

  (* (*  (- x2 x1) (- y2 y1))
     (monte-carlo trials test)))

;------------------estimate-pi----------------------

(define (estimate-pi trials)
  (define (P x y)
    (< (+ (square x) (square y)) 1.0))
  (estimate-integral P -1 1 -1 1 trials))

 (estimate-pi 1000)
;Value: 3.08
(estimate-pi 10000)
;Value: 3.1328
(estimate-pi 100000)
;Value: 3.1534
(estimate-pi 1000000)
;Value: 3.143396