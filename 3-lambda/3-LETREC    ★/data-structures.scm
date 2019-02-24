(module data-structures (lib "eopl.ss" "eopl")  
  (require "lang.scm")
  
  (provide (all-defined-out))
  (define identifier? symbol?)
  ; =========================================================== ExpVal
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p Proc?))
    )

  ; ExpVal -> number
  (define (expval->num expval)
    (cases ExpVal expval
      ($num-val (n) n)
      (else (eopl:error "Can't get num-val from ExpVal :" expval))
      ))
  ; ExpVal -> boolean
  (define (expval->bool expval)
    (cases ExpVal expval
      ($bool-val (b) b)
      (else (eopl:error "Can't get bool-val from ExpVal :" expval))
      ))
  ; ExpVal -> Proc
  (define (expval->proc expval)
    (cases ExpVal expval
      ($proc-val (p) p)
      (else (eopl:error "Can't get proc-val from ExpVal :" expval))
      ))

  ; =========================================================== Proc
  (define-datatype Proc Proc?
    ($procedure
     (var symbol?)
     (body Expression?)
     (env Env?)))

  ; =========================================================== Env ::= { symbol ~~ ExpVal }
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?) ; include Proc
     (env Env?))
    ($extend-env-rec
     (fid identifier?)
     (bvar identifier?)
     (proc-body Expression?)
     (env Env?))     
    )

  (define (init-env)
    ($extend-env 'i ($num-val 1)
                 ($extend-env 'v ($num-val 5)
                              ($extend-env 'x ($num-val 10)
                                           ($empty-env)))))

  (define (apply-env env var)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'apply-env "Empty env while search ~s" var))
      ($extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))
      ($extend-env-rec (fid bvar proc-body env/without-fid)                       
                       (if (eqv? fid var)
                           ($proc-val ($procedure bvar proc-body env))  ; ███ 不能是saved-env (不含fid) ,而必须是env
                           (apply-env env/without-fid var)))
      ))

  ; extend-env* :: [symbol] x [ExpVal] -> Env
  (define (extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          (extend-env* (cdr vars) (cdr expvals) new-env))))      

  )
