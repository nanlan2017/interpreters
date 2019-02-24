#lang eopl
(provide (all-defined-out))
(require "lang.rkt")
(require "data-structures.rkt")
;;=============================================================

;; eval :: expression x Env -> ExpVal    // 为什么结果类型不包括一个Env ?
(define (eval exp env)
  (cases expression exp
    (const-exp (n)
               ($num-val n))
    (var-exp (x)
             (apply-env env x))
    (diff-exp (e1 e2)
              ($num-val (- (expval->num (eval e1 env))        ;; 之所以同时求值：是因为 e1,e2 本来就会使用同一个 env (不同于带store?)
                           (expval->num (eval e2 env)))))
    
    (if-exp (b-e1 e2 e3)
            (let [(v1 (eval-bool-exp b-e1 env))]
              (if (expval->bool v1)
                  (eval e2 env)
                  (eval e3 env))))
    (let-exp (var e1 body)
             (let [(v1 (eval e1 env))]
               (eval body ($extend-env var v1 env))))

    (minus-exp (e1)
               ($num-val (- 0 (expval->num (eval e1 env)))))
    (add-exp (e1 e2)
             ($num-val (+ (expval->num (eval e1 env))
                          (expval->num (eval e2 env)))))
    (mult-exp (e1 e2)
              ($num-val (* (expval->num (eval e1 env))
                           (expval->num (eval e2 env)))))
    (quotient-exp (e1 e2)
                  ($num-val (quotient (expval->num (eval e1 env))
                                      (expval->num (eval e2 env)))))

    ;; cons
    (cons-exp (e1 e2)
              ($cons-val (eval e1 env) (eval e2 env)))
    (car-exp (exp)
             (get-head (eval exp env)))
    (cdr-exp (exp)
             (get-tail (eval exp env)))
    (null?-exp (exp)
               (let [(v (eval exp env))]
                 (cases ExpVal v
                   ($emptylist ()        ;; datatype的空构造函数也要 ()
                               ($bool-val #t))
                   (else ($bool-val #f)))))
    (empty-list-exp  (key)               ;; 这个key会匹配到token : emptylist
                     ($emptylist))
    ;; list
    (list-exp (exprs) ;; 类型：list-of expression?
              (if (null? exprs)
                  ($emptylist)
                  ($cons-val (eval (car exprs) env)
                             (eval (list-exp (cdr exprs)) env))))  ;; 此处必须用list-exp重新Wrap起来！
    (cond-exp (tests exps)
              (if (null? tests)
                  (eopl:error "No case matching for cond-exp!")
                  (if (expval->bool (eval-bool-exp (car tests) env))
                      (eval (car exps) env)
                      (eval (cond-exp (cdr tests) (cdr exps)) env))))

    ))

;; eval : bool-exp
(define (eval-bool-exp exp env)
  (cases Bool-exp exp
    (const-true ()
                ($bool-val #t))
    (const-false ()
                 ($bool-val #f))
    (zero?-exp (e1)
               (let* [(v1 (eval e1 env))
                      (v2 (expval->num v1))]
                 ($bool-val (if (= 0 v2) #t #f))))
    ))

;; eval : print
(define (eval-print-exp pexp env)
  (cases Print-exp pexp
    (a-print-exp (exp)
                 (let [(v (eval exp env))]
                   (begin
                     (eopl:printf "_P_R_I_N_T_ :~s~n" v)
                     1)))))

;;;;;;;;;;    eval : program
(define (eval-program prog)
  (cases program prog
    (a-program (expr)
               (eval expr (init-env)))
    (b-program (b-expr)
               (eval-bool-exp b-expr (init-env)))
    (prt-program (p-expr)
                 (eval-print-exp p-expr (init-env)))))
;;=============================================================  
;; :: String -> ExpVal
(define (interp src)
  (eval-program (scan&parse src)))

(define (interp2 src) 
  (eopl:printf "------------------------------~n~s~n~n~n~s~n" src (interp src)))

  
