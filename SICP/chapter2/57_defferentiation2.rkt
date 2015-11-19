;-----------------primitive definitions--------------------
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? exp) (and (pair? exp) (eq? '+ (car exp))))
;(define (make-sum v1 v2) (list '+ v1 v2))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? exp) (and (pair? exp) (eq? '* (car exp))))
;(define (make-product m1 m2) (list '* m1 m2))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (filter predicate? seq)
  (if (null? seq) '()
      (if (predicate? (car seq))
          (cons (car seq) (filter predicate? (cdr seq)))
          (filter predicate? (cdr seq)))))

;---------------procedures to filter the expressions-------------
(define (non-num-members sum-exp)
  (filter (lambda (x) (not (number? x))) sum-exp))

(define (num-members sum-exp)
  (filter number? sum-exp))

(define (more-than-one-number? sum-exp)
  (let ((nums (num-members sum-exp)))
    (if (or (null? nums) (null? (cdr nums))) #f #t)))

(define (zero-is-the-only-number? sum-exp)
  (let ((nums (num-members sum-exp)))
    (if (null? nums) #f
        (and (= (car nums) 0) (null? (cdr nums))))))

(define (one-is-the-only-number? sum-exp)
  (let ((nums (num-members sum-exp)))
    (if (null? nums) #f
        (and (= (car nums) 1) (null? (cdr nums))))))

;------------------make sum---------------------
(define (make-sum . as)
  (cond ((null? as) 0)
        ((null? (cdr as)) (car as))
        ((null? (non-num-members as)) (apply + as))
        ((more-than-one-number? as)
         (apply make-sum (append (non-num-members as)
                                 (list (apply + (num-members as))))))
        ((zero-is-the-only-number? as)
         (apply make-sum (non-num-members as)))
        (else (append '(+) as))))


;-----------------make product----------------------
(define (make-product . ms)
  (cond ((null? ms) 1)
        ((null? (cdr ms)) (car ms))
        ((null? (non-num-members ms)) (apply * ms))
        ((more-than-one-number? ms)
         (apply make-product (append (non-num-members ms)
                                     (list (apply * (num-members ms))))))
        ((zero-is-the-only-number? ms) 0)
        ((one-is-the-only-number? ms) (apply make-product (non-num-members ms)))
        (else (append '(*) ms))))




;---------------the main derivative procedure----------------
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                               (make-sum (exponent exp) -1))
                                     (deriv (base exp) var))))
        (else (error "unknown expression type -- DERIV" exp))))

;-------------make expomentiation-----------
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))