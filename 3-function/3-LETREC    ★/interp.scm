(module interp (lib "eopl.ss" "eopl")
  (require "lang.scm")
  (require "data-structures.scm")

  (provide (all-defined-out))
  ; ===================================================================

  (define (apply-procedure proc argv)
    (cases Proc proc
      ($procedure (var body-exp saved-env)
                  (value-of body-exp ($extend-env var argv saved-env)))))
  ; ===================================================================
  (define (interp src)
    (value-of-program (scan&parse src)))
  
  (define run interp)

  (define (value-of-program prog)
    (cases Program prog
      ($a-program (expr)
                  (value-of expr (init-env)))))

  (define (value-of exp env)
    (cases Expression exp
      ($const-exp (n)
                  ($num-val n))
      ($var-exp (x)
                (apply-env env x))
      ($diff-exp (e1 e2)
                 ($num-val (- (expval->num (value-of e1 env))
                              (expval->num (value-of e2 env)))))
      ($zero?-exp (e1)
                  (let* [(v1 (value-of e1 env))
                         (v2 (expval->num v1))]
                    ($bool-val (if (= 0 v2) #t #f))))
      ($if-exp (e1 e2 e3)
               (let [(v1 (value-of e1 env))]
                 (if (expval->bool v1)
                     (value-of e2 env)
                     (value-of e3 env))))
      ($let-exp (var e1 body)
                (let [(v1 (value-of e1 env))]
                  (value-of body ($extend-env var v1 env))))
      ($proc-exp (var body)
                 ($proc-val ($procedure var body env)))
      ($call-exp (rator rand)
                 (let [(f (expval->proc (value-of rator env)))
                       (arg (value-of rand env))]
                   (apply-procedure f arg)))

      ; ███ 求值letrec-body时,要让 fid~ Proc放在Env中(因为不同于let,letrec-body中会使用fid)
      ($letrec-exp (fid bvar proc-body letrec-body)
                   (value-of letrec-body ($extend-env-rec fid bvar proc-body env)))
                   ; x  $procedure ..env (this env doesn't include fid)
                   ; (value-of letrec-body ($extend-env fid ($proc-val ($procedure bvar proc-body env)) env))                   
                  
      ))

  
  ; ===================================================================
  

  )
