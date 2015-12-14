(define (make-monitored f)
  (let ((num_of_calls 0))
  (define (reset-count num_of_calls)
    (begin (set! num_of_calls 0)
           0))

  (define (how-many-calls? num_of_calls)
    num_of_calls)

  (define (dispatch m)
    (cond ((eq? m 'reset) (reset-count num_of_calls))
          ((eq? m 'how-many-calls?) (how-many-calls? num_of_calls))
          (else (begin (set! num_of_calls (+ num_of_calls 1))
                       (f m)))))
  dispatch))

;----------test values------------

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)


