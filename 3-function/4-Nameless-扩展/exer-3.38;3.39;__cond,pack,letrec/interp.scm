(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  ;;============================================================= procedure  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  (define (apply-procedure proc arg)
    (cases Proc proc
      ($procedure (body-exp env)
                  (value-of body-exp (extend-env arg env)))))
  
  ;;============================================================= trans
  ;; ★★★     _0, _1 分别对应该处的 senv 中的第0个符号、第1个符号

  
  ;; trans :: expression x SEnv -> Nameless-Exp
  ;; ██████████ 其实很简单：想象你给任何一个简单的expression去nameless化，此时必定要存在自由变量的上下文 senv ===> 通过索引来指明引用哪个变量
  (define (trans exp senv)
    (cases expression exp
      (const-exp (n)
                 (const-exp n))      
      (diff-exp (e1 e2)
                (diff-exp (trans e1 senv) (trans e2 senv)))
      (zero?-exp (e1)
                 (zero?-exp (trans e1 senv)))
      (if-exp (e1 e2 e3)
              (if-exp (trans e1 senv) (trans e2 senv) (trans e3 senv)))      
      (call-exp (rator rand)
                (call-exp (trans rator senv) (trans rand senv)))
      ; █--█--█
      (pack-exp (exps)
                (pack-exp (map (lambda (e) (trans e senv)) exps)))
      ;;`````````````````````````` Node -> Nameless Node
      (var-exp (x)
               (nameless-var-exp (apply-senv senv x)))
      (let-exp (var e1 body)
               (nameless-let-exp (trans e1 senv)
                                 (trans body (extend-senv var senv))))           ; let 会创建新binding
      (proc-exp (var body)
                (nameless-proc-exp (trans body (extend-senv var senv))))         ; proc 会创建新binding (declaration)
      ; █--█--█
      (unpack-exp (vars lst-val-exp body-exp)
                  (nameless-unpack-exp (trans lst-val-exp senv) (trans body-exp (extend-senv* vars senv))))
      ;;``````````````````````````
      (nameless-var-exp (idx)
                        (eopl:error "Nameless var shouldn't appear in trans"))
      (nameless-let-exp (exp1 body)
                        (eopl:error "Nameless let shouldn't appear in trans"))
      (nameless-proc-exp (body)
                         (eopl:error "Nameless proc shouldn't appear in trans"))
      ; █--█--█
      (nameless-unpack-exp (lst-val-exp body)
                         (eopl:error "Nameless unpack shouldn't appear in trans"))
      ))

  (define (trans-program prog)
    (cases program prog
      (a-program (expr)
                 (a-program (trans expr (init-senv))))))
  ;; nameless的AST
  (define (namelessfy src)
    (trans-program (scan&parse src)))
    
    
  ;;============================================================= eval
  ;; ★★★     _0, _1 分别去用 env中对应索引的值
  
  ;; ★ SEnv = [i, v ,x ]
  ;; ★  Env = [1, 5 ,10]
  
  ;; eval :: Nameless-Exp x Env -> ExpVal
  (define (value-of exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))      
      (diff-exp (e1 e2)
                ($num-val (- (expval->num (value-of e1 env))
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
      (call-exp (rator rand)
                (let [(f (expval->proc (value-of rator env)))
                      (arg (value-of rand env))]
                  (apply-procedure f arg)))
      ; █--█--█
      (pack-exp (exps)
                ($list-val (map (lambda (e) (value-of e env)) exps)))
      ;;``````````````````````````
      (var-exp (x)
               (eopl:error "Non-Nameless var shouldn't appear in eval"))
      (let-exp (var e1 body)
               (eopl:error "Non-Nameless let shouldn't appear in eval"))
      (proc-exp (var body)
                (eopl:error "Non-Nameless proc shouldn't appear in eval"))
      ; █--█--█
      (unpack-exp (vars lst-val-exp body-exp)
                  (eopl:error "Non-Nameless unpack shouldn't appear in eval"))
      
      ;;``````````````````````````
      (nameless-var-exp (idx)
                        (apply-env env idx))
      (nameless-let-exp (exp1 body)        ;; 新增binding
                        (let [(v1 (value-of exp1 env))]
                          (value-of body (extend-env v1 env))))
      (nameless-proc-exp (body)
                         ($proc-val ($procedure body env)))  ;; Wrap成ExpVal
      ; █--█--█
      (nameless-unpack-exp (lst-val-exp body)
                         (let [(vals (value-of lst-val-exp env))]
                           (value-of body (extend-env* (expval->list vals) env))))
      ))

  ;; eval-program :: Nameless-Program -> ExpVal
  (define (eval-program prog)
    (cases program prog
      (a-program (nameless-expr)
                 (value-of nameless-expr (init-env)))))
   
  ;; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (namelessfy src)))
  )
