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
     (vars (list-of symbol?))
     (body Expression?)
     (env Env?)))

  ; =========================================================== Env ::= { symbol ~~ ExpVal }
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?) ; include Proc
     (env Env?))
    ($extend-env-rec*
     (fids (list-of identifier?))  ; 让几个fs处于同一个层次里！
     (bvars-s (list-of (list-of identifier?)))
     (body-s (list-of Expression?))
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
      ($extend-env-rec* (fids bvars-s proc-body-s env/without-fids)                       
                        ;(if (eqv? fid var)
                        ;    ($proc-val ($procedure bvars proc-body env))  ; 不能是saved-env (不含fid) ,而必须是env
                        ;    (apply-env env/without-fid var)))             ; 这地方又丢了一个fid了！无法mutually recursive了！
                        (let [(res (maybe-has-proc? var fids bvars-s proc-body-s))]
                          (if res
                              ($proc-val ($procedure (cadr res) (caddr res) env)) ; ███ 这个env是mutually recursive的关键。
                              (apply-env env/without-fids var))))
      ))
  
  ; return '(fid bvars body) if exists ; else return #f                                    
  (define (maybe-has-proc? search-f fid-s bvars-s body-s)
    (if (null? fid-s)
        #f
        (if (eqv? search-f (car fid-s))
            (list search-f (car bvars-s) (car body-s))
            (maybe-has-proc? search-f (cdr fid-s) (cdr bvars-s) (cdr body-s)))))
                                      

  ; extend-env* :: [symbol] x [ExpVal] -> Env
  (define (extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          (extend-env* (cdr vars) (cdr expvals) new-env))))  

  )
