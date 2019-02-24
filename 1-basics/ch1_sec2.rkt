#lang racket

;; 一种Lambda表达式 数据结构实现 + 实现通用接口
;; 1.constructors
(define (var-exp var)
  (cons 'var var))

(define (lambda-exp var lcexp)
  (list 'lambda var lcexp))

(define (app-exp lcexp1 lcexp2)
  (list 'app lcexp1 lcexp2))

;; 2.predicts  ()
(define (var-exp? exp)
  (eqv? 'var (car exp)))

(define (lambda-exp? exp)
  (eqv? 'lambda (car exp)))

(define (app-exp? exp)
  (eqv? 'app (car exp)))
  
;; 3.extractors (可以提取λ expr的各个部分)
(define (var-exp->var exp)
  (cdr exp))

(define (lambda-exp->bound-var exp)
  (cadr exp))

(define (lambda-exp->body exp)
  (caddr exp))    ;;  cddr '(x y z) -->  '(z)              |  caddr '(x y z) --> 'z

(define (app-exp->rator exp)
  (cadr exp))

(define (app-exp->rand exp)
  (caddr exp))

;;===================================================================      
;; Lambda表达式数据类型的 通用接口
(define (occurs-free? search-var exp)
  (cond
    [(var-exp? exp) (eqv? search-var (var-exp->var exp))]
    [(lambda-exp? exp) (and (not (eqv? search-var (lambda-exp->bound-var exp)))
                            (occurs-free? search-var (lambda-exp->body exp)))]
    [else (or (occurs-free? search-var (app-exp->rator exp))
              (occurs-free? search-var (app-exp->rand exp)))]))
;;===================================================================
; x                      #t
(define lep0 (var-exp 'x))
; y                      #f
(define lep1 (var-exp 'y))
; \x -> x y              #f
(define lep2 (lambda-exp 'x '(x y)))
; \y -> x y              #t
(define lep3 (lambda-exp 'y '(x y)))
; (\x -> x) (x y)        #t
(define lep4 (app-exp (lambda-exp 'x 'x) '(x y)))
; \y -> \z -> x (y z)    #t
(define lep5 (lambda-exp 'y (lambda-exp 'z '(x (y z)))))

(occurs-free? 'x lep0)
(occurs-free? 'x lep1)
(occurs-free? 'x lep2)
(occurs-free? 'x lep3)
(occurs-free? 'x lep4)
(occurs-free? 'x lep5)














































