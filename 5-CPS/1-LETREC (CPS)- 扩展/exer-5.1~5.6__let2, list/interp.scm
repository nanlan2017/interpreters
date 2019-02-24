(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;;============================================================= procedure  
  ;; apply-procedure : Proc * Val * Cont -> FinalAnswer
  (define (apply-procedure/k proc arg cont)
    (cases Proc proc
      ($procedure (param bodyexp env)
                  (eval/k bodyexp ($extend-env param arg env) cont))))

  ;;============================================================ Continuation
  (define-datatype Continuation Continuation?
    ($end-cont)
    ; zero?-exp
    ($zero?-exp-cont
     (cont Continuation?))
    ; let-exp
    ($let-exp-cont
     (var identifier?)
     (body expression?)
     (env Env?)
     (cont Continuation?))
    ; if-exp
    ($if-test-cont
     (then-exp expression?)
     (else-exp expression?)
     (env Env?)
     (cont Continuation?))
    ; diff-exp
    ($diff1-cont
     (e2 expression?)
     (env Env?)
     (cont Continuation?))
    ($diff2-cont
     (v1 ExpVal?)
     (cont Continuation?))
    ; call-exp
    ($rator-cont
     (rand-exp expression?)
     (env Env?)
     (cont Continuation?))
    ($rand-cont
     (f-expval ExpVal?)
     (cont Continuation?))
    ; ----------------
    ; let2
    ($let2-1-cont
     (var1 identifier?)
     (var2 identifier?)
     (exp2 expression?)
     (body expression?)
     (env Env?)
     (cont Continuation?))
    ($let2-2-cont
     (var1 identifier?)
     (v1 ExpVal?)
     (var2 identifier?)
     (body expression?)
     (env Env?)
     (cont Continuation?))
    ; list
    ($list-car-cont  ; no vals yet
     (exps (list-of expression?))
     (env Env?)
     (cont Continuation?))
    ($list-rest-cont ; has vals
     (vals (list-of ExpVal?))
     (rest-exps (list-of expression?))
     (env Env?)
     (cont Continuation?))
    )

  ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k VAL) ; val : 上一步的计算结果
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 VAL)
      
      ($zero?-exp-cont (cont)
                       (apply-cont cont ($bool-val (zero? (expval->num VAL))))) ; VALUE : zero?(e)  中e的值
      
      ($let-exp-cont (var body env cont)
                     (eval/k body ($extend-env var VAL env) cont))          ; VALUE : let x = e 中e的值
      
      ($if-test-cont (then-exp else-exp env cont)
                     (if (expval->bool VAL)                                 ; VALUE : if e ..   中e的值
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      
      ($diff1-cont (e2 env cont)
                   (eval/k e2 env ($diff2-cont VAL cont)))                  ; VALUE : -(e1,e2)  中e1的值
      
      ($diff2-cont (v1 cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num VAL))))); VALUE : -(e1,e2)  中e2的值
      
      ($rator-cont (rand-exp env cont)
                   (eval/k rand-exp env ($rand-cont VAL cont)))             ; VALUE : (e1 e2)   中e1的值
      
      ($rand-cont (f-expval cont)                                             
                  (let [(f (expval->proc f-expval))]
                    (apply-procedure/k f VAL cont)                          ; VALUE : (e1 e2)   中e2的值
                    ; (apply-cont cont (apply-procedure f VALUE))
                    ))
      ; ----------------
      ($let2-1-cont (var1 var2 e2 body env cont)
                    (eval/k e2 env ($let2-2-cont var1 VAL var2 body env cont)))
      ($let2-2-cont (var1 v1 var2 body env cont)
                    (eval/k body (extend-env* (list var1 var2) (list v1 VAL) env) cont))
      ; ----------------
      ($list-car-cont (exps env cont)
                      (if (null? exps)
                          (apply-cont cont ($list-val (list VAL)))
                          (eval/k (car exps) env ($list-rest-cont (list VAL) (cdr exps) env cont))))
      ($list-rest-cont (vals exps env cont)
                       (let [(acc-vals (append vals (list VAL)))]
                         (if (null? exps)
                             (apply-cont cont ($list-val acc-vals))
                             (eval/k (car exps) env ($list-rest-cont acc-vals (cdr exps) env cont)))))
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
      
      (zero?-exp (e1)
                 (eval/k e1 env ($zero?-exp-cont cont)))
      
      (let-exp (var e1 body)
               (eval/k e1 env ($let-exp-cont var body env cont)))
      
      (if-exp (e1 e2 e3)
              (eval/k e1 env ($if-test-cont e2 e3 env cont)))
      
      (diff-exp (e1 e2)
                (eval/k e1 env ($diff1-cont e2 env cont)))
      
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont)))
      ; ----------------
      (let2-exp (var1 e1 var2 e2 body)
                (eval/k e1 env ($let2-1-cont var1 var2 e2 body env cont)))
      ; ----------------
      (list-exp (exps)
                (if (null? exps)
                    (apply-cont cont ($list-val '()))
                    (eval/k (car exps) env ($list-car-cont (cdr exps) env cont))))
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
