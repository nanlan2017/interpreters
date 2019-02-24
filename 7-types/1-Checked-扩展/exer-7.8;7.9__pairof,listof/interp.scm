(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")

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
      ($let-exp (var e1 body)
                (let [(v1 (eval e1 env))]
                  (eval body ($extend-env var v1 env))))
      ;; 1-arg proc
      ($proc-exp (var ty body)
                 ($proc-val ($procedure var body env)))
      ($call-exp (rator rand)
                 (let [(f (expval->proc (eval rator env)))
                       (arg (eval rand env))]
                   (apply-procedure f arg)))                               
      ;; letrec
      ($letrec-exp (p-res-type pid b-var b-var-type p-body letrec-body)
                   (eval letrec-body ($extend-env-rec pid b-var p-body env)))
      ; ---------------------------------
      ($newpair-exp (e1 e2)
                    (let ([v1 (eval e1 env)]
                          [v2 (eval e2 env)])
                      ($pair-val v1 v2)))
      
      ($unpair-exp (var1 var2 e1 body)
                   (let* [(v (eval e1 env))
                          (lv (expval->pair-left v))
                          (rv (expval->pair-right v))]
                     (eval body (extend-env* (list var1 var2) (list lv rv) env))))
      ; ---------------------------------
      ($list-exp (e1 exps)
                 (let [(vals (map (lambda (e) (eval e env)) (cons e1 exps)))]
                   ($list-val vals)))
      
      ($cons-exp (e1 exp)
                 (let [(v1 (eval e1 env))
                       (vs (eval exp env))]
                   ($list-val (append v1 (expval->list vs)))))
      
      ($null?-exp (exp)
                  (let [(val (eval exp env))]
                    (cases ExpVal val
                      ($list-val (vs)
                                 ($bool-val (null? vs)))
                      (else (eopl:error 'null?-exp "exp not a list-val")))))
      
      ($emptylist-exp (ty)
                      ($list-val '()))
                  
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
