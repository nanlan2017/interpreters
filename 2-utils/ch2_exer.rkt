#lang racket

;;=====================================================================================Exercise 2.1
; zero
; is-zero?
; succ
; pre

(define base 16)
(define (bzero) '())
(define bzero? null?)

(define (bsucc n)
  (if (bzero? n)
      '(1)
      (let [(t (+ 1 (car n)))]
        (if (= t base)
            (cons 0 (bsucc (cdr n)))
            (cons t (cdr n))))))

(define bpred
  (lambda (n)
    (cond
      ((bzero? n) #f)
      ((>= (car n) base) #f)
      ((equal? n '(1)) '())
      ((zero? (car n))
       (if (null? (cdr n))
           #f
           (cons (- base 1) (bpred (cdr n)))))
      (else (cons (- (car n) 1) (cdr n))))))

;-------------test
(bsucc '(1 2))
;;=====================================================================================Exercise 2.3  diff-tree representation
;;  val = 1  |  - m n
;;  0 = 1 - 1 |  2 - 2  | 3 - 3 ...
;;=====================================================================================Exercise 2.21


      
