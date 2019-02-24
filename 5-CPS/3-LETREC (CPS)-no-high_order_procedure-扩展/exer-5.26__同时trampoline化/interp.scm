(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
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
 
  ; ***************************************************************************************
  ;        【将tail call的参数进行register-iazation、均变为无参函数 的一般模式】
  ;
  ;        tail call :             g() = { ...; (f arg);}                f(x) = ..x..
  ;           变为   : global-x;   g() = { ...; set global-x arg; (f)}   f()  = ..global-x..
  ; ***************************************************************************************

  ; 3个来回跳转的函数
  ;  (eval/k             exp  env cont)
  ;  (apply-cont         cont val)
  ;  (apply-procedure/k  proc val cont)
  
  ; 引入5个shared variable
  (define %exp 'uninitialized)
  (define %env 'uninitialized)
  
  (define %cont 'uninitialized)
  (define %val 'uninitialized)
  
  (define %proc 'uninitialized)
  
  ; 另一种trampoline方式 : %pc 保存下一个需要执行的函数的地址
  ;    某些步骤不是直接去进入下一个函数、而是把下一步放在pc、自身交回控制权给 driver
  (define %PC #t)

  (define (fire-driver)
    (if %PC
        (begin
          (eopl:printf "[bouncing...]~n")
          (%PC)
          (fire-driver))
        %val))
  
  ; apply-procedure/k : () -> FinalAnswer
  ; 【tail call】 : eval/k
  (define (====>apply-procedure/k)
    (cases Proc %proc
      ($procedure (param bodyexp env)
                  (set! %exp bodyexp)
                  (set! %env ($extend-env param %val env))
                  (====>eval/k))))

  ; apply-cont :: ()-> FinalAnswer
  ; 【tail call】 : △  | eval/k  | apply-procedure/k
  (define (====>apply-cont)
    (cases Continuation %cont
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 (set! %PC #f)  ;█████
                 ; %val
                 )
      ($zero?-exp-cont (cont)
                       (set! %cont cont)
                       (set! %val ($bool-val (zero? (expval->num %val))))
                       (====>apply-cont))
      ($let-exp-cont (var body env cont)
                     (set! %cont cont)
                     (set! %exp body)
                     (set! %env ($extend-env var %val env))
                     (====>eval/k))
      ($if-test-cont (then-exp else-exp env cont)
                     (set! %cont cont)
                     (if (expval->bool %val)                            
                         (set! %exp then-exp)
                         (set! %exp else-exp))
                     (set! %env env)
                     (====>eval/k))
      ($diff1-cont (e2 env cont)
                   (set! %cont ($diff2-cont %val cont))
                   (set! %exp e2)
                   (set! %env env)
                   (====>eval/k))             
      ($diff2-cont (v1 cont)
                   (set! %cont cont)
                   (set! %val ($num-val (- (expval->num v1) (expval->num %val))))
                   (====>apply-cont))
      ($rator-cont (rand-exp env cont)
                   (set! %cont ($rand-cont %val cont))
                   (set! %exp rand-exp)
                   (set! %env env)
                   (====>eval/k))        
      ($rand-cont (f-expval cont)
                  (set! %cont cont)
                  (set! %proc (expval->proc f-expval))
                  (set! %val %val)   ; ?
                  ;(====>apply-procedure/k)
                  (set! %PC ====>apply-procedure/k) ;█████
                  )                                        
      ))  
   
  ; eval :: () -> FinalAnswer
  ; 【tail call】 : apply-cont  | △
  (define (====>eval/k)
    (cases expression %exp
      (const-exp (n)
                 (set! %val ($num-val n))
                 (====>apply-cont))
      (var-exp (x)
               (set! %val (apply-env %env x))
               (====>apply-cont))
      (proc-exp (var body)
                (set! %val ($proc-val ($procedure var body %env)))
                (====>apply-cont))
      (letrec-exp (pid b-var p-body letrec-body)
                  (set! %exp letrec-body)
                  (set! %env ($extend-env-rec pid b-var p-body %env))
                  (====>eval/k))
      (zero?-exp (e1)
                 (set! %cont ($zero?-exp-cont %cont))
                 (set! %exp e1)
                 (====>eval/k))
      (let-exp (var e1 body)
               (set! %cont ($let-exp-cont var body %env %cont))
               (set! %exp e1)
               (====>eval/k))
      (if-exp (e1 e2 e3)
              (set! %cont ($if-test-cont e2 e3 %env %cont))
              (set! %exp e1)
              (====>eval/k))
      (diff-exp (e1 e2)
                (set! %cont ($diff1-cont e2 %env %cont))
                (set! %exp e1)
                (====>eval/k))      
      (call-exp (rator rand)
                (set! %cont ($rator-cont rand %env %cont))
                (set! %exp rator)
                (====>eval/k))
      ))

  ; eval-program :: Program -> FinalAnswer
  (define (eval-program prog)
    (cases program prog
      (a-program (expr)
                 (set! %cont ($end-cont))
                 (set! %exp expr)
                 (set! %env (init-env))
                 ;█████
                 (set! %PC ====>eval/k)
                 (fire-driver))))
  ; =============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))

  
  (define run interp)

  )
