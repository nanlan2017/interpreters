(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")

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

  ; ___________________________________________________________________________________________________________
  ; ___________________________________________________________________________________________________________
  (define ($end-cont)
    (cons (lambda (val)
            val)
          (lambda (val)
            (eopl:error 'apply-handler "uncaught exception!"))))
  ;````````````````````````
  (define ($try-cont var handler-exp env cont)
    (let ([handler (lambda (val)
                     (eval/k handler-exp ($extend-env var val env) cont))])
      (cons (lambda (val)
              (apply-cont (cons (car cont) handler) val))
            handler)))

  (define ($raise1-cont saved-cont)
    (cons (cdr saved-cont)
          (cdr saved-cont)))
  ;````````````````````````
  ; diff
  (define ($diff1-cont exp2 env cont)
    (cons (lambda (val)
            (eval/k exp2 env (diff2-cont val cont)))
          (cdr cont)))

  (define ($diff2-cont val1 cont)
    (cons (lambda (val)
            (let ([n1 (expval->num val1)]
                  [n2 (expval->num val)])
              (apply-cont cont ($num-val (- n1 n2)))))
          (cdr cont)))
  ; unary-op
  (define ($unary-arg-cont unop cont)
    (cons (lambda (val)
            (apply-cont cont (apply-unary-op unop val)))
          (cdr cont)))
  ; if-exp
  (define (if-test-cont exp2 exp3 env cont)
    (cons (lambda (val)
            (if (expval->bool val)
                (eval/k exp2 env cont)
                (eval/k exp3 env cont)))
          (cdr cont)))
  ; call-exp
  (define ($rator-cont rand env cont)
    (cons (lambda (val)
            (eval/k rand env (rand-cont val cont)))
          (cdr cont)))

  (define ($rand-cont val1 cont)
    (cons (lambda (val)
            (let ([proc (expval->proc val1)])
              (apply-procedure/k proc val cont)))
          (cdr cont)))


  ; ___________________________________________________________________________________________________________
 
  ; eval :: Expression x Env x Cont -> FinalAnswer
  (define (eval/k exp env cont)
    (cases expression exp
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      (var-exp (x)
               (apply-cont cont (apply-env env x)))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      (const-list-exp (nums)
                      (apply-cont cont ($list-val (map $num-val nums))))      
      (letrec-exp (pid b-var p-body letrec-body)
                  (eval/k letrec-body ($extend-env-rec pid b-var p-body env) cont))      
      (let-exp (var e1 body)
               (eval/k (call-exp (proc-exp var body) e1) env cont))
      ; ----------------
      (if-exp (e1 e2 e3)
              (eval/k e1 env ($if-test-cont e2 e3 env cont)))
      (diff-exp (e1 e2)
                (eval/k e1 env ($diff1-cont e2 env cont)))      
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont)))
      (unary-op-exp (unary-op exp)
                    (eval/k exp env ($unary-arg-cont unary-op cont)))
      
      ; Exception Handling
      (try-exp (exp1 cvar contvar handler-exp)
               (eval/k exp1 env ($try-cont cvar contvar handler-exp env cont)))
      
      (raise-exp (exp1)
                 (eval/k exp1 env ($raise1-cont cont)))
      ))

   ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k VAL) 
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 VAL)          
      ($try-cont (cvar contvar handler-exp env cont)
                 (apply-cont cont VAL))  
      ($raise1-cont (cont)
                    (apply-handler VAL cont cont))
      ; `````````````````````````````````
      ; if
      ($if-test-cont (then-exp else-exp env cont)
                     (if (expval->bool VAL)                           
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      ; diff-exp
      ($diff1-cont (e2 env cont)
                   (eval/k e2 env ($diff2-cont VAL cont)))              
      ($diff2-cont (v1 cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num VAL)))))
      ; call-exp
      ($rator-cont (rand-exp env cont)
                   (eval/k rand-exp env ($rand-cont VAL cont)))         
      ($rand-cont (ratorval cont)
                  (cases ExpVal ratorval
                    ($proc-val (f)
                               (apply-procedure/k f VAL cont))
                    ($cont-val (ct)
                               (apply-cont ct VAL))   ; ████████████
                    (else (eopl:error "Operator of Call-exp Must be either a procedure or a continuation"))
                    ))
      ; unaryop
      ($unary-arg-cont (op cont)
                       (apply-cont cont (apply-unary-op op VAL)))
      ))
  ; ___________________________________________________________________________________________________________
  ; ___________________________________________________________________________________________________________
  ; ___________________________________________________________________________________________________________

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
