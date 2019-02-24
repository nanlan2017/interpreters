#lang racket

;; empty-stack
;; top
;; push, pop

;;;=================== 说白了，就是用一个“闭包函数对象” 来表示data

(define (empty-stack)
  (λ (cmd)
    (cond
      [(eqv? cmd 'top) (error "Try top on empty stack")]
      [(eqv? cmd 'pop) (error "Try pop from empty stack")]
      [else (error "unknown cmd on stack")])))

(define (push val stack)
  (λ (cmd)
    (cond
      [(eqv? cmd 'top) val]
      [(eqv? cmd 'pop) stack]
      [else (error "unknown cmd on stack")])))

(define jpop
  (lambda (stack)
    (stack 'pop)))

(define jtop
  (lambda (stack)
    (stack 'top)))

;;```````````````````````````````````
(define e (empty-stack))
(define x1 (push e 1))
(define x2 (push x1 2))
(define x3 (push x2 3))

;(eqv? (top (pop x2)) 1)
;(= (top x2) 2)
;(= (top x3) 3)
;(= (top (pop (pop x3))) 1)

