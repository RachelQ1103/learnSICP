(define zero (lambda (f) (lambda(x) x)))

(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (f (lambda (x) x))))

(define two (lambda (f) (f (f (lambda (x) x)))))

(define (plus n m)
  (lambda (f) (lambda (x) ((m f)((n f) x)))))