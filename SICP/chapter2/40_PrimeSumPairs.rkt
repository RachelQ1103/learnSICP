; -------- <make-unique-pairs> --------
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

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (x) (list i x))
                (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; -------- <prime?> --------
(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; -------- <make-prime-sum-pairs> --------

(define (filter predicate? seq)
  (if (null? seq) nil
      (if (predicate? (car seq))
          (cons (car seq) (filter predicate? (cdr seq)))
          (filter predicate? (cdr seq)))))

(define (prime-sum? seq)
  (prime?  (+ (car seq) (cadr seq))))

(define (filter-prime-sum seq)
  (filter prime-sum? seq))

(define (make-sum-pair pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (make-prime-sum-pairs n)
  (map (lambda (pair) (make-sum-pair pair))
   (filter-prime-sum (unique-pairs n))))


  




  
