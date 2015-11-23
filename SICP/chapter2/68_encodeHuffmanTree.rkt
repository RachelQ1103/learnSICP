;--------------encoding--------------

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;------------encode the message by a given tree----------

(define (encode message tree)
  (if (null? message) '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;--------------68 encode-symbol------------------

(define (encode-symbol letter tree)
  (define (element-of-symbols? letter symbols)
    (cond ((null? symbols) #f)
          ((equal? letter (car symbols)) #t)
          (else (element-of-symbols? letter (cdr symbols)))))

  (define (lookup letter tree result)
    (let* ((left (left-branch tree))
           (left-symbols (symbols left)))
      (if (element-of-symbols? letter left-symbols)
          (if (leaf? left)
              (append result (list 0))
              (lookup letter left (append result (list 0))))
          (let* ((right (right-branch tree))
                 (right-symbols (symbols right)))
            (if (element-of-symbols? letter right-symbols)
                (if (leaf? right)
                    (append result (list 1))
                    (lookup letter right (append result (list 1))))
                (error "unknow symbol: " letter))))))
  (lookup letter tree '()))

;-------------the sample tree & message---------------

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(a d a b b c a))

;----------------test values-----------------
;(encode '(a d a b b c a) sample-tree)
;(encode '(a e a b d c) sample-tree)
