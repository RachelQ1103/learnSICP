(define (make-account balance password)
  (let ((incorrect-p 0))
    
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (display-error amount)
    "Incorrect password")

  (define (call-the-cops amount)
    (display "call the cops"))

  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (begin (set! incorrect-p (+ 1 incorrect-p))
               (if (> incorrect-p 7)
                   call-the-cops
                   display-error))))
  dispatch))
;-------test values----------
(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;60

((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
