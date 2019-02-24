(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  (require "data-structures-continuation.scm")
  ; 【思路】
  ;       eval exp env (diff2 -1    (diff1    (zero    (try ## (let v (end)))))
  ;  优化为
  ;       eval exp env (diff2 -1 ## (diff1 ## (zero ## (try ## (let v (end)))))
  ;;============================================================= procedure  
  ;; apply-procedure : Proc * Val * Cont -> FinalAnswer
  (define (apply-procedure/k proc arg cont)
    (cases Proc proc
      ($procedure (param bodyexp env)
                  (eval/k bodyexp ($extend-env param arg env) cont))))

  ; apply-unary-op :: UnaryOp * ExpVal -> ExpVal
  (define (apply-unary-op op val)
    (cases unary-op op
      (zero?-op ()
                ($bool-val (zero? (expval->num val))))
      (null?-op ()
                ($bool-val (null? (expval->list val))))
      (car-op ()
              (car (expval->list val)))
      (cdr-op ()
              ($list-val (cdr (expval->list val))))
      ))








 

  
  ; apply-handler :: ExpVal x Cont -> FinalAnswer
  (define (app-handler val k resume-cont)
    (cases Continuation k      
      ($end-cont ()
                 (eopl:error 'app-handler "======= Uncaught Exception!~s~n" val))
      ($try-cont (cvar handler-exp env cont with-try-cont)
                 (eval/k handler-exp ($extend-env cvar val env) resume-cont with-try-cont))
      (else (eopl:error 'apply-handler "not a try-cont!"))))


  ;;============================================================= Cont
  ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k VAL)
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 VAL)          
      
      ($try-cont (cvar handler-exp env cont with-try-cont)
                 (apply-cont cont VAL))
      
      ($raise-cont (with-try-cont resume-cont)
                   (app-handler VAL with-try-cont))
      ; `````````````````````````````````
      ; if
      ($if-test-cont (then-exp else-exp env cont with-try-cont)
                     (if (expval->bool VAL)                           
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      ; diff-exp
      ($diff1-cont (e2 env cont with-try-cont)
                   (eval/k e2 env ($diff2-cont VAL cont with-try-cont)))              
      ($diff2-cont (v1 cont with-try-cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num VAL)))))
      ; call-exp
      ($rator-cont (rand-exp env cont with-try-cont)
                   (eval/k rand-exp env ($rand-cont VAL cont with-try-cont)))         
      ($rand-cont (f-expval cont with-try-cont)                                             
                  (apply-procedure/k (expval->proc f-expval) VAL cont))
      ; unaryop
      ($unary-arg-cont (op cont with-try-cont)
                       (apply-cont cont (apply-unary-op op VAL)))
      ))
    
  
  ;;============================================================= eval    
  ; eval :: Expression x Env x Cont -> FinalAnswer
  (define (eval/k exp env cont)
    (cases expression exp
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      (var-exp (x)
               (apply-cont cont (apply-env env x)))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
            
      (letrec-exp (pid b-var p-body letrec-body)
                  (eval/k letrec-body ($extend-env-rec pid b-var p-body env) cont))
      (const-list-exp (nums)
                      (apply-cont cont ($list-val (map $num-val nums))))
      
      ; let x = e1 in body  ===  ((\x->body) e1)
      (let-exp (var e1 body)
               (eval/k (call-exp (proc-exp var body) e1) env cont))
      ; ----------------
      (if-exp (e1 e2 e3)
              (eval/k e1 env ($if-test-cont e2 e3 env cont (lift-try cont))))
      (diff-exp (e1 e2)
                (eval/k e1 env ($diff1-cont e2 env cont (lift-try cont))))      
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont (lift-try cont))))
      (unary-op-exp (unary-op exp)
                    (eval/k exp env ($unary-arg-cont unary-op cont (lift-try cont))))
      
      ; ----------------
      (try-exp (exp1 cvar handler-exp)
               (eval/k exp1 env ($try-cont cvar handler-exp env cont)))
      
      (raise-exp (exp1)
                 (eval/k exp1 env ($raise-cont (lift-try cont)))) ; ★
      ))

  ; eval-program :: Program -> FinalAnswer
  (define (eval-program prog)
    (cases program prog
      (a-program (expr)
                 (eval/k expr (init-env) ($end-cont)))))
  ; =============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))
  (define run interp)

  )
