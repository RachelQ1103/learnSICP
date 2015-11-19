(define nil '())

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mob)
  (car mob))

(define (right-branch mob)
  (cadr mob))

(define (branch-length x)
  (car x))

(define (branch-structure x)
  (cadr x))

(define (total-weight mob)
  (define (weight mob length)
    (if (null? mob) 0
        (if (not (pair? (branch-structure mob)))
            (* length (branch-structure mob))
            (+ (weight (left-branch (branch-structure mob)) (+ length (branch-length (left-branch (branch-structure mob)))))
               (weight (right-branch (branch-structure mob)) (+ length (branch-length (right-branch (branch-structure mob)))))))))
  (weight (list 0 mob) 0))       


; test total-weight      
(define m1 (make-mobile (make-branch 1 48)
                        (make-branch 4 12)))
(total-weight m1)
;Value: 60

(define m2 (make-mobile (make-branch 6 m1) 
                        (make-branch 9 40)))
(total-weight m2)
;Value: 100

(define m3 (make-mobile (make-branch 3 140) 
                        (make-branch 7 60)))
(total-weight m3)
;Value: 200

(define m4 (make-mobile (make-branch 20 m2)
                        (make-branch 10 m3)))
(total-weight m4)