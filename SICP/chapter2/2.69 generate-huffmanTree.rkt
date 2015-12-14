;----------------make-leaf-set----------------------

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;-----------------make-leaf-----------------
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;---------------make-code-tree-------------
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

;----------------successive-merge leaf-set-------------

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((node1 (car leaf-set))
            (node2 (cadr leaf-set))
            (rest (cddr leaf-set)))
        (let ((new-node (make-code-tree node1 node2)))
          (successive-merge (adjoin-set new-node rest))))))

;----------69 generate-huffman-tree pairs--------------

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;------------test values--------------------
(define pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
;(generate-huffman-tree pairs)