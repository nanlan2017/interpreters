(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "2-data-structures.scm")

  ;;============================================================= procedure  
  ;; apply-procedure : Proc * Val -> ExpVal
  (define (apply-procedure proc arg)
    (cases Proc proc
      ($procedure (param bodyexp env)
                  (eval bodyexp ($extend-env param arg env)))))  
  
  ;;============================================================= 
  ;; eval :: expression x Env -> ExpVal
  (define (eval exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))
      (var-exp (x)
               (apply-env env x))
      (diff-exp (e1 e2)
                (let [(num1 (expval->num (eval e1 env)))
                      (num2 (expval->num (eval e2 env)))]
                  ($num-val (- num1 num2))))
      (zero?-exp (e1)
                 (let [(v1 (expval->num (eval e1 env)))]
                   (if (zero? v1)
                       ($bool-val #t)
                       ($bool-val #f))))
      (if-exp (e1 e2 e3)
              (if (expval->bool (eval e1 env))
                  (eval e2 env)
                  (eval e3 env)))
      (let-exp (var e1 body)
               (let [(v1 (eval e1 env))]
                 (eval body ($extend-env var v1 env))))
      ;; proc
      (proc-exp (var ty body)
                ($proc-val ($procedure var body env)))
      (call-exp (rator rand)
                (let [(f (expval->proc (eval rator env)))
                      (arg (eval rand env))]
                  (apply-procedure f arg)))                               
      ;; letrec
      (letrec-exp (p-res-type pid b-var b-var-type p-body letrec-body)
                  (eval letrec-body ($extend-env-rec pid b-var p-body env)))
                  
      ))

  ; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (cases program prog
      (a-program (expr)
                 (eval expr (init-env)))))
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))
  (define run interp)

  )
