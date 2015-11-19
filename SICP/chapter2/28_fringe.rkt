(define nil '())

(define (fringe items)
  (cond ((null? items) nil)
        ((not (pair? (car items)))
         (append (list (car items)) (fringe (cdr items))))
        (else (append (fringe (car items))
              (fringe (cdr items))))))