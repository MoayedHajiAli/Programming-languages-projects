#lang racket

;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;

(define (fibonacci n)
    (define (fib-cont1 n cont)
        (lambda (x) (fib n (fib-cont2 x cont))))
    
    (define (fib-cont2 n cont)
        (lambda (x) (cont (+ x n))))

    (define (fib n cont)
        (cond 
        ((= 1 n) (cont 1))
        ((= 2 n) (cont 1))
        (else (fib (- n 1) (fib-cont1 (- n 2) cont)))))

    (fib n (lambda (x) x))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; i:    1  2  3  4  5  6  7  8  9  10 11 ...
; f(i): 1  1  2  3  5  8  13 21 34 55 ...

;; Tests
(display (fibonacci 4)) ; should output 3
(display  "\n")
(display (fibonacci 7)) ; should output 13
(display  "\n")
(display (fibonacci 8)) ; should output 21
(display  "\n")