(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "2-data-structures.scm")

  ;;============================================================= procedure  
  ;; apply-procedure : Proc * Val -> ExpVal
  (define (apply-procedure proc arg)
    (cases Proc proc
      ($procedure (param bodyexp env)
                  (eval bodyexp ($extend-env param arg env)))))

  ;;=============================================================
  ; add-module-defns-to-env :: [ModuleDefinition] * Env -> Env
  (define (add-module-defns-to-env defns env)
    (if (null? defns)
        env
        (cases ModuleDefinition (car defns)
          ($a-module-definition (m-name iface m-body)
                                (let [(new-env ($extend-env-with-module m-name (eval-module-body m-body iface env) env))]  ; 有误，
                                  (add-module-defns-to-env (cdr defns) new-env))))))
  
  ; eval-module-body :: ModuleBody * Interface * Env -> TypedModule
  (define (eval-module-body m-body iface env)
    (cases ModuleBody m-body
      ($a-module-body (var-defs)
                      ($a-simple-module (limit-to-interface iface (defns-to-env var-defs env))))))

  ; defns-to-env : Listof(Defn) × Env → Env
  (define (defns-to-env defns env)
    (if (null? defns)
        ($empty-env)
        (cases VarDefinition (car defns)
          ($a-var-definition (var exp)
                             (let ((val (eval exp env)))
                               (let ((new-env ($extend-env var val env)))
                                 ($extend-env var val (defns-to-env (cdr defns) new-env))))))))

  (define (limit-to-interface iface env-of-defns)
    (let [(i-vars (pick-iface-vars iface))
          (pair-lst (env-to-list env-of-defns))]
      (let loop [(pair-lst pair-lst)]
        (if (null? pair-lst)
            ($empty-env)
            (let [(pr (car pair-lst))]
              (if (memq (car pr) i-vars)
                  ($extend-env (car pr) (cdr pr) (loop (cdr pair-lst)))
                  (loop (cdr pair-lst))))))))
      

  (define (env-to-list env)
    (cases Env env
      ($empty-env ()
                  '())
      ($extend-env (var val saved-env)
                   (cons (cons var val) (env-to-list saved-env)))
      (else (env-to-list (get-nested-env env)))))
                  

  ; pick-iface-vars :: Interface -> [symbol]
  (define (pick-iface-vars iface)
    (cases SimpleInterface iface
      ($a-simple-interface (var-decls)
                           (if (null? var-decls)
                               '()
                               (cases VarDeclaration (car var-decls)
                                 ($a-var-declaration (id ty)
                                                     (cons id (pick-iface-vars ($a-simple-interface (cdr var-decls))))))))))
                   

  
  ;;=============================================================
  ; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (cases Program prog
      ($a-program (mod-defs expr)
                  (eval expr (add-module-defns-to-env mod-defs (init-env))))))
  
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))
  (define run interp)
  
  ;; eval :: expression x Env -> ExpVal
  (define (eval exp env)
    (cases Expression exp
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
      ($proc-exp (var ty body)
                 ($proc-val ($procedure var body env)))
      ($call-exp (rator rand)
                 (let [(f (expval->proc (eval rator env)))
                       (arg (eval rand env))]
                   (apply-procedure f arg)))                               
      ($letrec-exp (p-res-type pid b-var b-var-type p-body letrec-body)
                   (eval letrec-body ($extend-env-rec pid b-var p-body env)))
      ; ----------------------------
      ($qualified-var-exp (mod-name var-name)
                          (lookup-qualified-var-in-env mod-name var-name env))
                  
      ))


  )
