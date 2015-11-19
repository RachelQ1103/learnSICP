(define (same-parity g . w)
  (define filter-predicate? (if (odd? g) odd? even?))

  (define (iter-parity items)
    (if (null? items)
        '()
        (if (filter-predicate? (car items))
            (cons (car items) (iter-parity (cdr items)))
            (iter-parity (cdr items)))))

  (cons g (iter-parity w)))