;--------------list-manipulations------------------

(define nil '())
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low (enumerate-interval (+ 1 low) high))))

(define (filter predicate? seq)
  (if (null? seq) nil
      (if (predicate? (car seq))
          (cons (car seq) (filter predicate? (cdr seq)))
          (filter predicate? (cdr seq)))))

(define (last sequence)
  (car (reverse sequence)))

;---------queens board-size, all solutions---------

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;-----------board positions-------------------------

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

(define empty-board '() )

;--------------------check safe?---------------------

(define (safe? k positions)
  (define (two-queens-safe? q1 q2)
    (not (or (= (car q1) (car q2))
             (= (cadr q1) (cadr q2))
             (= (+ (car q1) (cadr q1)) (+ (car q2) (cadr q2)))
             (= (- (car q1) (cadr q1)) (- (car q2) (cadr q2))))))
  (let ((new-queen (last positions)))
    (define (check i positions)
     (cond ((= i k) #t)
          ((two-queens-safe? new-queen (car positions))
           (check (+ i 1) (cdr positions)))
          (else #f)))
    (check 1 positions)))
  