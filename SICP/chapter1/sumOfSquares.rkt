(define (sum-of-bigger-squares x y z)
  (cond ((and (< x y)(< x z))
         (+ (* y y)(* z z)))
         (else (sum-of-bigger-squares y z x))))