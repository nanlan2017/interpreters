(module interp-CPS-OUT (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang-in-out.scm")
  (require "transform-CPS-IN-to-CPS-OUT.scm")
  (require "data-structures.scm")
  ;====================================================================
  (define (apply-procedure f args)
    (cases Proc f
      ($procedure (vars body saved-env)
                  (eval body (extend-env* vars args saved-env)))))
  ;====================================================================
  (define (run-cps-src src)
    (eval-program (cps-of-program (scan&parse src))))
  
  (define eval-program 
    (lambda (pgm)
      (cases CpsProgram pgm
        (a-cps-program (exp1)
                       (eval exp1 (init-env))))))

  (define eval-simple
    (lambda (exp env)
      (cases SimpleExp exp
        (@const-exp (num) ($num-val num))
        
        (@var-exp (var) (apply-env env var))

        (@diff-exp (exp1 exp2)
                   (let [(val1 (expval->num (eval-simple exp1 env)))
                         (val2 (expval->num (eval-simple exp2 env)))]
                     ($num-val (- val1 val2))))

        (@zero?-exp (exp1)
                    ($bool-val (zero? (expval->num (eval-simple exp1 env)))))

        (@sum-exp (exps)
                  (let ((numbers (map (lambda (exp) (expval->num (eval-simple exp env))) exps)))
                    ($num-val
                     (let sum-loop ((nums numbers))
                       (if (null? nums)
                           0
                           (+ (car nums) (sum-loop (cdr nums))))))))

        (@proc-exp (vars body)
                   ($proc-val ($procedure vars body env)))

        )))

  ; ★eval/k :: TfExp -> Env -> Cont -> FinalAnswer(ExpVal)
  ;     因为都是tail-form expression，所以cont都是一样的！因而可以彻底移除
  (define (eval exp env)
    (cases TailExp exp
      ($simple-exp->exp (simple)
                        (eval-simple simple env))
        
      ($let-exp (var rhs body)
                (let ((val (eval-simple rhs env)))
                  (eval body (extend-env* (list var) (list val) env))))
        
      ($letrec-exp (p-names b-varss p-bodies letrec-body)
                   (eval letrec-body (extend-env-rec* p-names b-varss p-bodies env)))
        
      ($if-exp (simple1 body1 body2)
               (if (expval->bool (eval-simple simple1 env))
                   (eval body1 env)
                   (eval body2 env)))
        
      ($call-exp (rator rands)
                 (let ((rator-proc (expval->proc (eval-simple rator env)))
                       (rand-vals (map (lambda (simple) (eval-simple simple env)) rands)))
                   (apply-procedure rator-proc rand-vals)))
      ))

  )
