(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "1-data-structures.scm")
  (require "1-store.scm")
  (require "utils.scm")
  ;;============================================================= procedure  
  ;; apply-procedure : Proc * [ExpVal] -> ExpVal
  (define (apply-procedure proc argv-s)
    (cases Proc proc
      ($procedure (var-s body-exp saved-env)
                  (value-of body-exp (extend-env* var-s argv-s saved-env)))))

  ;;============================================================= 
  ;; eval :: expression x Env -> ExpVal
  (define (value-of exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))
      (var-exp (x)
               (apply-env env x))
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
      (let-exp (var e1 body)
               (let [(v1 (value-of e1 env))]
                 (value-of body ($extend-env var v1 env))))
      ;`````````````````````````````````````````````````
      (proc-exp (vars body)
                ($proc-val ($procedure vars body env)))
      (call-exp (rator rands)
                (let [(f (expval->proc (value-of rator env)))
                      (argvs (map (lambda (rand) (value-of rand env)) rands))]
                  (apply-procedure f argvs)))
      
      (letrec-exp (fid-s bvar-s-s proc-body-s letrec-body)
                   (value-of letrec-body ($extend-env-rec* fid-s bvar-s-s proc-body-s env)))

      (begin-exp (exps)
                 (let [(vals (map (lambda (e) (value-of e env)) exps))]
                   (list-last vals)))
      ;`````````````````````````````````````````````````
      (newref-exp (exp1)
                  (let [(v1 (value-of exp1 env))]
                    ($ref-val (newref v1))))
      (deref-exp (refexp1)
                 (let [(rv1 (expval->ref (value-of refexp1 env)))]
                   (deref rv1)))
      (setref-exp (refexp exp1)
                  (let [(rv (expval->ref (value-of refexp env)))
                        (v1 (value-of exp1 env))]
                    (begin
                      (setref! rv v1)
                      'any-value)))
          
      ))

  ;; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (initialize-store!)
    (cases program prog
      (a-program (expr)
                 (value-of expr (init-env)))))
  ;;=============================================================  
  ;; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define run interp)

  )
