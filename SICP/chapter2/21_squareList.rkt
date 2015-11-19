(define square
    (lambda (x) (* x x)))

(define (square-list items)
  (define nil '())
  (if (null? items) nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))