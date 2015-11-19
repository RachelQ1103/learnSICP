(define nil '())

(define (reversed items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items) (cons (car items) result))))

  (reverse-iter items nil))

(define (deep-reverse items)
  (define (deep-reversed items)
    (if (null? items)
        nil
        (if (not (pair? (car items)))
            (cons (car items) (deep-reversed (cdr items)))
            (cons (deep-reversed (reversed (car items)))
                  (deep-reversed (cdr items))))))
  (deep-reversed (reversed items)))
      