(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "1-store.scm")
  (require "1-data-structures.scm")
  (require "utils.scm")

  ;============================================================ Proc
  ; apply-procedure : Proc * [ExpVal] -> ExpVal
  (define (apply-procedure/k proc arg cont)
    (cases Procedure proc
      ($procedure (var body env)
                  (eval/k body ($extend-env var (newref arg) env) cont))))

  ;============================================================
  (define (apply-cont cont VAL)
    (cont VAL))

  (define (apply-cmd-cont cont)
    (cont))

  ;============================================================= execute/k
  ; A program is a statement.
  ; A statement does not return a value, but acts by modifying the store and by printing
  (define (execute-program prog)
    (initialize-store!)
    (cases program prog
      (a-program (stat)
                 (execute/k stat (init-env) (lambda ()
                                              (eopl:printf "End of execution...~n"))))))

  ; execute/k :: Statement * Env -> ()
  (define (execute/k STAT env cont)
    (cases statement STAT
      (assign-stat (var exp)
                   (eval/k exp env (lambda (v1)
                                     (setref! (apply-env env var) v1)
                                     (apply-cmd-cont cont))))
      
      (print-stat (exp)
                  (eval/k exp env (lambda (v)
                                    (eopl:printf "[console]>>>>>>>>>>>> ~s~n" v)
                                    (apply-cmd-cont cont))))
      ;; ★
      (seq-stat (stat-s)
                (letrec ([make-cont (lambda (statements)
                                      (if (null? statements)
                                          (lambda () (apply-cmd-cont cont))
                                          (lambda () (execute/k (car statements) env (make-cont (cdr statements))))))])                  
                  (if (null? stat-s)
                      (apply-cmd-cont cont)
                      (execute/k (car stat-s) env (make-cont (cdr stat-s))))))

      
      (if-stat (exp then-stat else-stat)
               (eval/k exp env (lambda (b)
                                 (if (expval->bool b)
                                     (execute/k then-stat env cont)
                                     (execute/k else-stat env cont)))))
      ;; ★
      (while-stat (exp statement)
                  (letrec [(statement-cont (lambda ()
                                             (eval/k exp env (lambda (val)
                                                               (if (expval->bool val)
                                                                   (execute/k statement env statement-cont)
                                                                   (apply-cmd-cont cont))))))]
                    (apply-cmd-cont statement-cont)))

      ;; ★
      (block-stat (vars body)
                  (let ([new-env (let loop ([vars vars]
                                            [env env])
                                   (if (null? vars)
                                       env
                                       (loop (cdr vars) ($extend-env (car vars) (newref 'uninitialized) env))))])
                    (execute/k body new-env cont)))
      ))
  
  ;============================================================= value-of
  (define (eval/k exp env cont)
    (cases expression exp
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      
      (var-exp (x)
               (apply-cont cont (deref (apply-env env x))))

      (diff-exp (e1 e2)
                (eval/k e1 env (lambda (v1)
                                 (eval/k e2 env (lambda (v2)
                                                  (apply-cont cont ($num-val (- (expval->num v1) (expval->num v2)))))))))
      (add-exp (e1 e2)
               (eval/k e1 env (lambda (v1)
                                (eval/k e2 env (lambda (v2)
                                                 (apply-cont cont ($num-val (+ (expval->num v1) (expval->num v2)))))))))
      (mult-exp (e1 e2)
                (eval/k e1 env (lambda (v1)
                                 (eval/k e2 env (lambda (v2)
                                                  (apply-cont cont ($num-val (* (expval->num v1) (expval->num v2)))))))))

      (zero?-exp (e1)
                 (eval/k e1 env (lambda (v1)
                                  (apply-cont cont ($bool-val (zero? (expval->num v1)))))))
      (if-exp (e1 e2 e3)
              (eval/k e1 env (lambda (b)
                               (if (expval->bool b)
                                   (eval/k e2 env cont)
                                   (eval/k e3 env cont)))))
                                 
      (not-exp (e1)
               (eval/k e1 env (lambda (v1)
                                (apply-cont cont ($bool-val (not (expval->bool v1)))))))
      (let-exp (var e1 body)
               (eval/k e1 env (lambda (v1)
                                (eval/k body ($extend-env var v1 env) cont))))

      ; 1-parameter procedure
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      (call-exp (rator rand)
                (eval/k rator env (lambda (fval)
                                    (eval/k rand env (lambda (randval)
                                                       (apply-procedure/k (expval->proc fval) randval cont))))))                     
      ; letrec
      (letrec-exp (pid-s bvar-s pbody-s letrec-body)
                  (eval/k letrec-body ($extend-env-rec* pid-s bvar-s pbody-s env) cont))
      ; begin ???
      (begin-exp (exp-s)
                 (letrec [(make-cont (lambda (exprs)
                                       (if (null? exprs)
                                           (lambda (r) (apply-cont cont r))
                                           (lambda (r) (eval/k (car exprs) env (make-cont (cdr exprs)))))))]
                   (eval/k (car exp-s) env (make-cont (cdr exp-s)))))
      ; assignment
      (assign-exp (var exp1)
                  (eval/k exp1 env (lambda (v1)
                                     (setref! (apply-env env var) v1)
                                     (apply-cont cont 'void))))              
      ))

  ;=============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (execute-program (scan&parse src)))
  (define run interp)

  )
