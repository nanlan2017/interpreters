(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "0-lang.scm")
  (require "1-store.scm")
  (require "utils.scm")
  ;============================================================ Inner Value
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p proc?))
    )

  ; expval -> number
  (define (expval->num expval)
    (cases ExpVal expval
      ($num-val (n) n)
      (else (eopl:error "Can't get num-val from ExpVal :" expval))
      ))
  ; expval -> boolean
  (define (expval->bool expval)
    (cases ExpVal expval
      ($bool-val (b) b)
      (else (eopl:error "Can't get bool-val from ExpVal :" expval))
      ))
  ; expval -> Proc
  (define (expval->proc expval)
    (cases ExpVal expval
      ($proc-val (p) p)
      (else (eopl:error "Can't get proc-val from ExpVal :" expval))
      ))
  ;============================================================ Proc (part 1)
  (define-datatype Procedure proc?
    ($procedure
     (var identifier?)
     (body expression?)
     (env Env?)))
    
  ;============================================================ Env ★[Ref]
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val Ref?)  ; ★ 
     (env Env?))
    ($extend-env-rec*
     (p-name (list-of identifier?))
     (b-var (list-of identifier?))
     (body (list-of expression?))
     (env Env?))
    )

  (define (init-env)
    ($extend-env 'i (newref ($num-val 1))
                     ($extend-env 'v (newref ($num-val 5))
                                  ($extend-env 'x (newref ($num-val 10)) ($empty-env)))))
  
  ; apply-env :: Env -> Symbol -> Ref
  (define (apply-env env var)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'apply-env "Didn't find in env while search : ~s" var))
      ($extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))
      ($extend-env-rec* (pname-s bvar-s pbody-s saved-env)
                       (let [(n (location var pname-s))]
                         (if n
                             (newref ($proc-val (list-ref bvar-s n) (list-ref pbody-s n) env))  ; 查找到这个procedure (ExpVal)
                             (apply-env saved-env var))))
      ))

  ; $extend-env* :: [symbol] x [ExpVal] -> Env
  (define (extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          (extend-env* (cdr vars) (cdr expvals) new-env))))

  )
