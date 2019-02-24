(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")  
  ;============================================================ Continuation (part 1)
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
  ;=========================================================================  type Bounce = ExpVal | () → Bounce
  (define-datatype Bounce Bounce?                                   ; Bounce : 既能包含ExpVal的值、又能包含-可Wrap一个 ()->Bounce的函数即可
    ($final
     (val ExpVal?))
    ($bounce
     (p pair?))  ; (any,无参过程)
    )

  (define counter 0)
  (define (update-counter)
    (set! counter (+ 1 counter))
    counter)
  
  ; trampoline :: Bounce -> FinalAnswer 
  (define (trampoline bounce)
    (cases Bounce bounce
      ($final (val)
              val)
      ($bounce (pr)
               (begin
                 (eopl:printf "[bouncing...] ~s~n" (car pr))
                 (trampoline ((cdr pr)))))))

  ;=============================================================
  ; 注意： tail call on eval/k ,所以其返回类型必然同 eval/k的返回类型一致
  ; apply-procedure :: Proc x Val x Cont -> Bounce
  (define apply-procedure/k
    (let [(counter 0)]
      (lambda (proc arg cont)
        (cases Proc proc
          ($procedure (param bodyexp env)
                      (set! counter (+ 1 counter))                      
                      ($bounce (cons counter (lambda () ; ███
                                               (eval/k bodyexp ($extend-env param arg env) cont)))))))))
  
  
  ; 注意： tail call on eval/k 或 apply-cont 或 apply-procedure/k
  ; apply-cont :: Cont x ExpVal -> Bounce
  (define (apply-cont k VAL)
    ($bounce (cons 'apply-cont (lambda () ; ███
                                 (cases Continuation k
                                   ($end-cont ()
                                              (eopl:printf "End of Computation.~%")
                                              ($final VAL)) ; █ █ █
                                   ($zero?-exp-cont (cont)
                                                    (apply-cont cont ($bool-val (zero? (expval->num VAL)))))
                                   ($let-exp-cont (var body env cont)
                                                  (eval/k body ($extend-env var VAL env) cont))
                                   ($if-test-cont (then-exp else-exp env cont)
                                                  (if (expval->bool VAL)                     
                                                      (eval/k then-exp env cont)
                                                      (eval/k else-exp env cont)))
                                   ($diff1-cont (e2 env cont)
                                                (eval/k e2 env ($diff2-cont VAL cont)))   
                                   ($diff2-cont (v1 cont)
                                                (apply-cont cont ($num-val (- (expval->num v1) (expval->num VAL)))))
                                   ($rator-cont (rand-exp env cont)
                                                (eval/k rand-exp env ($rand-cont VAL cont)))       
                                   ($rand-cont (f-expval cont)                                             
                                               (let [(f (expval->proc f-expval))]
                                                 ($bounce (cons (update-counter) (lambda () ; ███
                                                                                   (apply-procedure/k f VAL cont))))))                                        
                                   )))))  
  
  ;============================================================= eval/k      
  ; 注意：均为对 apply-cont / eval/k 的tail call
  ; eval/k :: Expression x Env x Cont -> Bounce
  (define (eval/k exp env cont)
    (cases expression exp
      ;--------------------- no call on eval/k
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      (var-exp (x)
               (apply-cont cont (apply-env env x)))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      ;--------------------- call on eval/k
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
                 (trampoline (eval/k expr (init-env) ($end-cont))))))
  ; =============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))
  (define run interp)

  (define src-test-1     ; -> 177
    "(proc (f) (f (f 199))   
      proc (x) -(x,11))"
    )
  
  (define src-test-2
    "let f = proc (x) proc (y) -(x,y)
     in ((f 33) 4)"
    )

  )
