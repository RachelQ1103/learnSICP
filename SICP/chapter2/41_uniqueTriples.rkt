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

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (x) (append (list i) x))
                  (flatmap (lambda (y)
                             (map (lambda (z)(list y z))
                                  (enumerate-interval 1 (- y 1))))
                            (enumerate-interval 1 (- i 1)))))
           (enumerate-interval 1 n)))

(define (equal-sum? s)
  (lambda (triple) 
  (= s (+ (car triple) (cadr triple) (caddr triple)))))

(define (make-sum-triples triple)
  (list (car triple) (cadr triple) (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))))

(define (filter predicate? seq)
  (if (null? seq) nil
      (if (predicate? (car seq))
          (cons (car seq) (filter predicate? (cdr seq)))
          (filter predicate? (cdr seq)))))

(define (unique-equal-triples n s)
  (filter (equal-sum? s) (unique-triples n)))

(define (equal-sum-triples n s)
  (map make-sum-triples (unique-equal-triples n s)))


  