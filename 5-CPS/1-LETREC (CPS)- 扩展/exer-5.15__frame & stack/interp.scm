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

  (define (end-cont) '())
  
  (define-datatype frame frame?
    [zero1-frame]
    
    [let-exp-frame
     [var identifier?]
     [body expression?]
     [saved-env Env?]]
    
    [if-test-frame
     [exp2 expression?]
     [exp3 expression?]
     [saved-env Env?]]
    
    [diff1-frame
     [exp2 expression?]
     [saved-env Env?]]
    [diff2-frame
     [val1 ExpVal?]]
    
    [rator-frame
     [rand expression?]
     [saved-env Env?]]
    [rand-frame
     [val1 ExpVal?]])

  ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k VALUE)                    ; val : 上一步的计算结果
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 VALUE)
      ($zero?-exp-cont (cont)
                       (apply-cont cont ($bool-val (zero? (expval->num VALUE))))) 
      ($let-exp-cont (var body env cont)
                     (eval/k body ($extend-env var VALUE env) cont)) 
      ($if-test-cont (then-exp else-exp env cont)
                     (if (expval->bool VALUE)                            
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      ($diff1-cont (e2 env cont)
                   (eval/k e2 env ($diff2-cont VALUE cont)))             
      ($diff2-cont (v1 cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num VALUE)))))
      ($rator-cont (rand-exp env cont)
                   (eval/k rand-exp env ($rand-cont VALUE cont)))     
      ($rand-cont (f-expval cont)                                             
                  (let [(f (expval->proc f-expval))]
                    (apply-procedure/k f VALUE cont)))                                        
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
