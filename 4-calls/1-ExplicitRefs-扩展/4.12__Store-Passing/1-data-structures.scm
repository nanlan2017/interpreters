(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "0-lang.scm")
  (require "utils.scm")
  ;;====================================== Expressed Value | Denoted Value
  (define reference? integer?)
  
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p Proc?))
    ($ref-val
     (idx reference?))
    )

  ;; expval -> number
  (define (expval->num expval)
    (cases ExpVal expval
      ($num-val (n) n)
      (else (eopl:error "Can't get num-val from ExpVal :" expval))
      ))
  ;; expval -> boolean
  (define (expval->bool expval)
    (cases ExpVal expval
      ($bool-val (b) b)
      (else (eopl:error "Can't get bool-val from ExpVal :" expval))
      ))
  ;; expval -> Proc
  (define (expval->proc expval)
    (cases ExpVal expval
      ($proc-val (p) p)
      (else (eopl:error "Can't get proc-val from ExpVal :" expval))
      ))
  ;; expval -> Reference (integer?)
  (define (expval->ref expval)
    (cases ExpVal expval
      ($ref-val (r) r)
      (else (eopl:error "Can't get ref-val from ExpVal :" expval))
      ))
  ;;====================================== Proc (抽象类型)
  (define-datatype Proc Proc?
    ($procedure
     (bvars (list-of symbol?))
     (body expression?)
     (env Env?)))
    
  ;;====================================== Env (采用datatype 表示法)
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?)
     (env Env?))
    ($extend-env-rec*
     (fids (list-of identifier?))  ; 让几个fs处于同一个层次里!
     (bvars-s (list-of (list-of identifier?)))
     (body-s (list-of expression?))
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
