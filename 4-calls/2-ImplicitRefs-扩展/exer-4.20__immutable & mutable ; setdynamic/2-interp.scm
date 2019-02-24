(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "1-store.scm")
  (require "1-data-structures.scm")
  (require "utils.scm")
  ;============================================================ Proc (part 2)
  ; apply-procedure : Proc * [ExpVal] -> ExpVal
  (define (apply-procedure proc arg)
    (cases Procedure proc
      ($procedure (var body env)
                  (value-of body ($extend-env var (newref arg) env)))))  ; ████ call by value
  ;============================================================= eval
  ; eval :: expression x Env -> ExpVal
  (define (value-of exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))      
      (diff-exp (e1 e2)
                ($num-val (- (expval->num (value-of e1 env))
                             (expval->num (value-of e2 env)))))
      (add-exp (e1 e2)
               ($num-val (+ (expval->num (value-of e1 env))
                            (expval->num (value-of e2 env)))))
      (mult-exp (e1 e2)
                ($num-val (* (expval->num (value-of e1 env))
                             (expval->num (value-of e2 env)))))
      (zero?-exp (e1)
                 (let* [(v1 (value-of e1 env))
                        (v2 (expval->num v1))]
                   ($bool-val (if (= 0 v2) #t #f))))
      (if-exp (e1 e2 e3)
              (let [(v1 (value-of e1 env))]
                (if (expval->bool v1)
                    (value-of e2 env)
                    (value-of e3 env))))
      
      ;``````````````````````````````````````````````````````````````````  
      (var-exp (x)
               (let [(v (apply-env env x))]
                 (if (Ref? v)
                     (deref v)  ; mutable
                     v)))       ; immutable

      ; let immutable
      (let-exp (var e1 body)
               (let [(v1 (value-of e1 env))]
                 (value-of body ($extend-env var v1 env))))
      ; letmutable == 原来的let
      (letmutable-exp (var e1 body)
                      (let [(v1 (value-of e1 env))]
                        (value-of body ($extend-env var (newref v1) env))))
      ; assignment
      (assign-exp (var exp1)
                  (let [(v (apply-env env var))]
                    (if (Ref? v)
                        (setref! v (value-of exp1 env))
                        (eopl:error "Illegal to assign to an Immutable variable~s in ~s !~n" v exp))))
      
      (setdynamic-exp (var exp1 body)
                      (let* [(refv (apply-env env var))
                             (old-val (deref refv))
                             (new-val (value-of exp1 env))]
                        (begin
                          (setref! refv new-val)
                          (let [(result (value-of body env))]
                            (begin
                              (setref! refv old-val)
                              result)))))
      ;``````````````````````````````````````````````````````````````````  
      (proc-exp (var body)
                ($proc-val ($procedure var body env)))
      (call-exp (rator rand)
                (let [(f (expval->proc (value-of rator env)))
                      (arg (value-of rand env))]
                  (apply-procedure f arg)))                               
      ; letrec
      (letrec-exp (pid-s bvar-s pbody-s letrec-body)
                  (value-of letrec-body ($extend-env-rec* pid-s bvar-s pbody-s env)))
      ; begin
      (begin-exp (exps)
                 (let [(vals (map (lambda (e) (value-of e env)) exps))]
                   (list-last vals)))
                       
      ))

  ; eval-program :: Program -> ExpVal
  (define (value-of-program prog)
    (initialize-store!)
    (cases program prog
      (a-program (expr)
                 (value-of expr (init-env)))))
  ;=============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (value-of-program (scan&parse src)))

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))

  (define sp scan&parse)
  (define run interp)

  )
