(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  ;;=============================================================

  ;; eval :: expression x Env -> ExpVal    // 为什么结果类型不包括一个Env ?
  (define (eval exp env)
    (cases Expression exp
      ($Expr.const-exp (n)
                 ($ExpVal.num-val n))
      ($Expr.var-exp (x)
               (apply-env env x))
      ($Expr.diff-exp (e1 e2)
                ($ExpVal.num-val (- (expval->num (eval e1 env))
                             (expval->num (eval e2 env)))))
      ($Expr.zero?-exp (e1)
                 (let* [(v1 (eval e1 env))
                        (v2 (expval->num v1))]
                   ($ExpVal.bool-val (if (= 0 v2) #t #f))))
      ($Expr.if-exp (e1 e2 e3)
              (let [(v1 (eval e1 env))]
                (if (expval->bool v1)
                    (eval e2 env)
                    (eval e3 env))))
      ($Expr.let-exp (var e1 body)
               (let [(v1 (eval e1 env))]
                 (eval body ($Env.extend-env var v1 env))))
      ))

  (define (eval-program prog)
    (cases Program prog
      ($Program.a-program (expr)
                 (eval expr (init-env)))))
  ;;=============================================================  
  ;; :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))
  (define run interp)

  (define (interp2 src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))

  )
