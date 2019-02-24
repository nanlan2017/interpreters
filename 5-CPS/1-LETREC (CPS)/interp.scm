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
    )

  ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k VALUE) ; val : 上一步的计算结果
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 VALUE)
      ($zero?-exp-cont (cont)
                   (apply-cont cont ($bool-val (zero? (expval->num VALUE))))) ; VALUE : zero?(e)  中e的值
      ($let-exp-cont (var body env cont)
                     (eval/k body ($extend-env var VALUE env) cont))          ; VALUE : let x = e 中e的值
      ($if-test-cont (then-exp else-exp env cont)
                     (if (expval->bool VALUE)                                 ; VALUE : if e ..   中e的值
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      ($diff1-cont (e2 env cont)
                   (eval/k e2 env ($diff2-cont VALUE cont)))                  ; VALUE : -(e1,e2)  中e1的值
      ($diff2-cont (v1 cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num VALUE))))); VALUE : -(e1,e2)  中e2的值
      ($rator-cont (rand-exp env cont)
                   (eval/k rand-exp env ($rand-cont VALUE cont)))             ; VALUE : (e1 e2)   中e1的值
      ($rand-cont (f-expval cont)                                             
                  (let [(f (expval->proc f-expval))]
                    (apply-procedure/k f VALUE cont)                          ; VALUE : (e1 e2)   中e2的值
                    ; (apply-cont cont (apply-procedure f VALUE))
                    ))                                        
      ))  
  
  ;;============================================================= eval  
  ; █████████ 重写目标： 所有对 eval/k的调用都属于tail call (doesn't build control context)
  ; █████████ f(x,cont)  !=  cont( f(x,end-cont) )
  
  ; eval :: Expression x Env x Cont -> FinalAnswer
  (define (eval/k exp env cont)
    (cases expression exp
      ;--------------------------------------------- no call on eval/k
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      (var-exp (x)
               (apply-cont cont (apply-env env x)))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      ;--------------------------------------------- 1 call on eval
      ; letrec : the inner-'eval' is in the same cont as caller-eval      
      ;      (letrec-exp (pid b-var p-body letrec-body)
      ;                  (eval letrec-body ($extend-env-rec pid b-var p-body env)))
      (letrec-exp (pid b-var p-body letrec-body)
                  (eval/k letrec-body ($extend-env-rec pid b-var p-body env) cont))
      
      ; zero?-exp : eval --> [ expval->num >>= zero? >>= $bool-val ] --> cont      
      ;      (zero?-exp (e1)
      ;                 (let [(☗ (eval e1 env))]
      ;                   ($bool-val (zero? (expval->num ☗)))))
      (zero?-exp (e1)
                 (eval/k e1 env ($zero?-exp-cont cont)))
      ;--------------------------------------------- 2 call on eval
      ;      (let-exp (var e1 body)
      ;               (let [(☗ (eval e1 env))]
      ;                 (eval body ($extend-env var ☗ env))))
      (let-exp (var e1 body)
               (eval/k e1 env ($let-exp-cont var body env cont)))
      
      ;      (if-exp (e1 e2 e3)
      ;              (if (expval->bool (eval e1 env))
      ;                  (eval e2 env)
      ;                  (eval e3 env)))
      (if-exp (e1 e2 e3)
              (eval/k e1 env ($if-test-cont e2 e3 env cont)))
      
      ;      (diff-exp (e1 e2)
      ;                (let [(num1 (expval->num (eval e1 env)))
      ;                      (num2 (expval->num (eval e2 env)))]
      ;                  ($num-val (- num1 num2))))
      (diff-exp (e1 e2)
                (eval/k e1 env ($diff1-cont e2 env cont)))      
      
      ;      (call-exp (rator rand)
      ;                (let [(f (expval->proc (eval rator env)))
      ;                      (arg (eval rand env))]
      ;                  (apply-procedure f arg)))
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont)))
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

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))
  (define run interp)

  )
