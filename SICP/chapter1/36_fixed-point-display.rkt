(define tolerance 0.00001)
(define (average a b)
  (/ (+ a b) 2))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (display-info guess step)
    (let ((next (f guess)))
      (if (close-enough? next guess) guess
          (try next (+ 1 step)))))
  (try first-guess 1))

(define (display-info guess step)
  (display "Step: ")
  (display step)
  (display " ")

  (display "Guess: ")
  (display guess)
  (newline))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define formula
  (lambda (x)
    (/ (log 1000) (log x))))