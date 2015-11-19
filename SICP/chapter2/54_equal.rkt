(define (equal2? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((eq? (car a) (car b)) (equal2? (cdr a) (cdr b)))
        ((not (eq? (car a) (car b))) #f)))