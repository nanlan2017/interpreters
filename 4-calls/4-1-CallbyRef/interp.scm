(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;============================================================ Proc (part 2)  
  ; apply-procedure-by-value : Proc * Ref -> ExpVal
  ;    无论call-by-value/ call-by-ref，都是接受Ref实参
  ;    把“决定Ref是原val的Ref/ 新分配的val的Ref”的职责交给caller处实现
  (define (apply-procedure proc argref)
    (cases Procedure proc
      ($procedure (var body env)
                  (eval body ($extend-env var argref env)))))
  ;============================================================= eval
  ; decide-arg :: Expression -> Env -> Ref
  (define (decide-operand rand-exp env)
    (cases expression rand-exp
      (var-exp (x)
               (let [(refv (apply-env env x))]
                 (eopl:printf "Invoking with argref: ~s , points to STORE :[~s] = ~s~n" x refv (deref refv))
                 refv))
      (else (newref (eval rand-exp env)))))
  
  ; eval :: expression x Env -> ExpVal
  (define (eval exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))
      (var-exp (x)
               (deref (apply-env env x)))
      (if-exp (e1 e2 e3)
              (let [(v1 (eval e1 env))]
                (if (expval->bool v1)
                    (eval e2 env)
                    (eval e3 env))))
      (let-exp (var e1 body)
               (let [(v1 (eval e1 env))]
                 (eval body ($extend-env var (newref v1) env))))
      ; built-in operators
      (diff-exp (e1 e2)
                ($num-val (- (expval->num (eval e1 env))
                             (expval->num (eval e2 env)))))
      (add-exp (e1 e2)
               ($num-val (+ (expval->num (eval e1 env))
                            (expval->num (eval e2 env)))))
      (mult-exp (e1 e2)
                ($num-val (* (expval->num (eval e1 env))
                             (expval->num (eval e2 env)))))
      (zero?-exp (e1)
                 (let* [(v1 (eval e1 env))
                        (v2 (expval->num v1))]
                   ($bool-val (if (= 0 v2) #t #f))))     
      ; 1-parameter procedure
      (proc-exp (var body)
                ($proc-val ($procedure var body env)))
      (call-exp (rator rand)
                (let [(f (expval->proc (eval rator env)))
                      (arg (decide-operand rand env))]  ; ★
                  (apply-procedure f arg)))                      
      ; letrec*
      (letrec*-exp (pid-s bvar-s pbody-s letrec-body)
                   (eval letrec-body ($extend-env-rec pid-s bvar-s pbody-s env)))
      ; begin
      (begin-exp (exps)
                 (let [(vals (map (lambda (e) (eval e env)) exps))]
                   (list-last vals)))
      ; assignment
      (assign-exp (var exp1)
                  (begin
                    (setref! (apply-env env var) (eval exp1 env))
                    'any-value))                  
      ))

  ; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (initialize-store!)
    (cases program prog
      (a-program (expr)
                 (eval expr (init-env)))))
  ;=============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))
  ; short names
  (define sp scan&parse)
  (define run interp)

  )
