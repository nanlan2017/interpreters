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
                 ($proc-val ($procedure var body (reduce-env body env))))  ; ██████ reduce-env
      ($call-exp (rator rand)
                 (let [(f (expval->proc (value-of rator env)))
                       (arg (value-of rand env))]
                   (apply-procedure f arg)))
                  
      ))

  ; ===================================================================
  ; 返回只包含body-exp中的free variable的env即可
  (define (reduce-env body-exp env)
    (let* [(fvs (free-variables body-exp '()))
           (rev-fvs (reverse fvs))]
      (build-reduced-env rev-fvs env)))

  (define (build-reduced-env rev-fvs env)
    (if (null? rev-fvs)
        ($empty-env)
        (let [(var1 (car rev-fvs))]
          ($extend-env var1 (apply-env env var1) (build-reduced-env (cdr rev-fvs) env)))))

  ; fv \x-> x+y+z  == '(y z)
  (define (free-variables expr bvar)
    (cases Expression expr
      ($const-exp (num)
                  '())
      ($var-exp (var)
                (if (memq var bvar)
                    '()
                    (list var)))
      ($diff-exp (exp1 exp2)
                 (append (free-variables exp1 bvar)
                         (free-variables exp2 bvar)))
      ($zero?-exp (arg)
                  (free-variables arg bvar))
      ($if-exp (pred consq alte)
               (append (free-variables pred bvar)
                       (free-variables consq bvar)
                       (free-variables alte bvar)))
      ($let-exp (var value body)
                (append (free-variables value bvar)
                        (free-variables body (cons var bvar))))
      ($proc-exp (var body)
                 (append (free-variables body (cons var bvar))))
      ($call-exp (var body)
                 (append (free-variables var bvar)
                         (free-variables body bvar)))
      ))
                

                
  )
