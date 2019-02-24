(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")

  ;;============================================================= procedure  
  ;; apply-procedure : Proc * Val -> ExpVal
  (define (apply-procedure proc argvs)
    (cases Proc proc
      ($procedure (params bodyexp env)
                  (eval bodyexp (extend-env* params argvs env)))))  
  
  ;;============================================================= 
  ;; eval :: expression x Env -> ExpVal
  (define (eval exp env)
    (cases expression exp
      ($const-exp (n)
                  ($num-val n))
      ($var-exp (x)
                (apply-env env x))
      ($diff-exp (e1 e2)
                 (let [(num1 (expval->num (eval e1 env)))
                       (num2 (expval->num (eval e2 env)))]
                   ($num-val (- num1 num2))))
      ($zero?-exp (e1)
                  (let [(v1 (expval->num (eval e1 env)))]
                    (if (zero? v1)
                        ($bool-val #t)
                        ($bool-val #f))))
      ($if-exp (e1 e2 e3)
               (if (expval->bool (eval e1 env))
                   (eval e2 env)
                   (eval e3 env)))
      ($let-exp (vars exps body)
                (let [(vals (map (lambda (e) (eval e env)) exps))]
                  (eval body (extend-env* vars vals env))))
      ;; 1-arg proc
      ($proc-exp (vars ty body)
                 ($proc-val ($procedure vars body env)))
      ($call-exp (rator rands)
                 (let [(f (expval->proc (eval rator env)))
                       (argvs (map (lambda (e) (eval e env)) rands))]
                   (apply-procedure f argvs)))                               
      ;; letrec
      ($letrec-exp (p-res-type-s pid-s bvars-s b-var-type-s-s pbody-s letrec-body)
                   (eval letrec-body ($extend-env-rec* pid-s bvars-s pbody-s env)))
                  
      ))

  ; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (cases program prog
      ($a-program (expr)
                  (eval expr (init-env)))))

  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))
  (define run interp)

  )
