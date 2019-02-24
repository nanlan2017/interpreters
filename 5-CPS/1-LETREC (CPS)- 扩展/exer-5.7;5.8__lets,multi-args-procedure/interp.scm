(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")

  (define largest-depth -1)
  
  (define (update-depth n)
    (if (> n largest-depth)
        (begin 
          (set! largest-depth n)
          n)
        n))
  ;;============================================================= procedure  
  ;; apply-procedure : Proc * Val * Cont -> FinalAnswer
  (define (apply-procedure/k proc args cont)
    (eopl:printf "apply-procedure/k :: depth of continuation: ~s~n" (update-depth (depth-of cont)))
    (cases Proc proc
      ($procedure (params bodyexp env)
                  (eval/k bodyexp (extend-env* params args env) cont))))

  ;;============================================================ Continuation
  (define (depth-of k)
    (cases Continuation k
      ($end-cont () 1)
      
      ($zero?-exp-cont (cont) (+ 1 (depth-of cont)))      
      ($if-test-cont (then-exp else-exp env cont) (+ 1 (depth-of cont)))      
      ($diff1-cont (e2 env cont) (+ 1 (depth-of cont)))                 
      ($diff2-cont (v1 cont) (+ 1 (depth-of cont)))
      
      ($rator-cont (rand-exp-s env cont) (+ 1 (depth-of cont)))      
      ($rands-car-cont (f randexp-s env cont) (+ 1 (depth-of cont)))      
      ($rands-rest-cont (f rand-vals randexp-s env cont) (+ 1 (depth-of cont)))

      ($let2-1-cont (var1 var2 e2 body env cont) (+ 1 (depth-of cont)))      
      ($let2-2-cont (var1 v1 var2 body env cont) (+ 1 (depth-of cont)))

      ($list-car-cont (exps env cont) (+ 1 (depth-of cont)))      
      ($list-rest-cont (vals exps env cont) (+ 1 (depth-of cont)))

      ($lets-car-cont (vars exps body env cont) (+ 1 (depth-of cont)))      
      ($lets-rest-cont (vars vals exps body env cont) (+ 1 (depth-of cont)))
      ))
  
  (define-datatype Continuation Continuation?
    ($end-cont)
    ; zero?-exp
    ($zero?-exp-cont
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
     (randexp-s (list-of expression?))
     (env Env?)
     (cont Continuation?))
    ($rands-car-cont
     (f Proc?)
     (randexp-s (list-of expression?))
     (env Env?)
     (cont Continuation?))
    ($rands-rest-cont
     (f Proc?)
     (randvals (list-of ExpVal?))
     (randexp-s (list-of expression?))
     (env Env?)
     (cont Continuation?))
    

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

    
    ; lets
    ($lets-car-cont
     (vars (list-of identifier?))
     (exps (list-of expression?))
     (body expression?)
     (env Env?)
     (cont Continuation?))
    ($lets-rest-cont
     (vars (list-of identifier?))
     (vals (list-of ExpVal?))
     (rest-exps (list-of expression?))
     (body expression?)
     (env Env?)
     (cont Continuation?))
    )

  ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k VAL) ; val : 上一步的计算结果
    (eopl:printf "apply-cont :: depth of continuation: ~s~n" (update-depth (depth-of k)))
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 (eopl:printf "最大的continuation深度：~s~n" largest-depth)
                 VAL)
      
      ($zero?-exp-cont (cont)
                       (apply-cont cont ($bool-val (zero? (expval->num VAL))))) ; VALUE : zero?(e)  中e的值
      
      ($if-test-cont (then-exp else-exp env cont)
                     (if (expval->bool VAL)                                 ; VALUE : if e ..   中e的值
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      
      ($diff1-cont (e2 env cont)
                   (eval/k e2 env ($diff2-cont VAL cont)))                  ; VALUE : -(e1,e2)  中e1的值
      
      ($diff2-cont (v1 cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num VAL))))); VALUE : -(e1,e2)  中e2的值
      ; ----------------
      ($rator-cont (rand-exp-s env cont)
                   (if (null? rand-exp-s)
                       (apply-procedure/k (expval->proc VAL) '() cont)
                       (eval/k (car rand-exp-s) env ($rands-car-cont (expval->proc VAL) (cdr rand-exp-s) env cont))))
      
      ($rands-car-cont (f randexp-s env cont)
                       (if (null? randexp-s)
                           (apply-procedure/k f (list VAL) cont)
                           (eval/k (car randexp-s) env ($rands-rest-cont f (list VAL) (cdr randexp-s) env cont))))
      
      ($rands-rest-cont (f rand-vals randexp-s env cont)
                        (let [(acc-vals (append rand-vals (list VAL)))]
                          (if (null? randexp-s)
                              (apply-procedure/k f acc-vals cont)
                              (eval/k (car randexp-s) env ($rands-rest-cont f acc-vals (cdr randexp-s) env cont)))))
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
      ;----------------------
      ($lets-car-cont (vars exps body env cont)
                      (if (null? exps)
                          (eval/k body env cont)
                          (eval/k (car exps) env ($lets-rest-cont vars (list VAL) (cdr exps) body env cont))))
      
      ($lets-rest-cont (vars vals exps body env cont)
                       (let [(acc-vals (append vals (list VAL)))]
                         (if (null? exps)
                             (eval/k body (extend-env* vars acc-vals env) cont)
                             (eval/k (car exps) env ($lets-rest-cont vars acc-vals (cdr exps) body env cont)))))
      ))  
  
  ;;============================================================= eval    
  ; eval :: Expression x Env x Cont -> FinalAnswer
  (define (eval/k exp env cont)
    (eopl:printf "eval/k :: depth of continuation: ~s~n" (update-depth (depth-of cont)))
    (cases expression exp
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      
      (var-exp (x)
               (apply-cont cont (apply-env env x)))
      
      (proc-exp (vars body)
                (apply-cont cont ($proc-val ($procedure vars body env))))
      
      (letrec-exp (pid b-var p-body letrec-body)
                  (eval/k letrec-body ($extend-env-rec pid b-var p-body env) cont))
      
      (zero?-exp (e1)
                 (eval/k e1 env ($zero?-exp-cont cont)))

      (if-exp (e1 e2 e3)
              (eval/k e1 env ($if-test-cont e2 e3 env cont)))
      
      (diff-exp (e1 e2)
                (eval/k e1 env ($diff1-cont e2 env cont)))     
      
      ; ----------------
      (let2-exp (var1 e1 var2 e2 body)
                (eval/k e1 env ($let2-1-cont var1 var2 e2 body env cont)))
      ; ----------------
      (list-exp (exps)
                (eval/k (car exps) env ($list-car-cont (cdr exps) env cont)))
      ;---------------------------------
      (lets-exp (vars exps body)
                (eval/k (car exps) env ($lets-car-cont vars (cdr exps) body env cont)))
      
      (call-exp (rator rand-exp-s)
                (eval/k rator env ($rator-cont rand-exp-s env cont)))
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
