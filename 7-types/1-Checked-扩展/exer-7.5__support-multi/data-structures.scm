(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  (require "utils.scm")
  ;;====================================== ExpVal
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p Proc?))
    )

  (define expval->num
    (lambda (v)
      (cases ExpVal v
        ($num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases ExpVal v
        ($bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases ExpVal v
        ($proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))
  ;;====================================== Proc
  (define-datatype Proc Proc?
    ($procedure
     (var (list-of identifier?))
     (body expression?)
     (env Env?)))   
  ;;====================================== Env
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?)
     (env Env?))
    ($extend-env-rec*
     (p-name (list-of identifier?))
     (b-var (list-of (list-of identifier?)))
     (body (list-of expression?))
     (env Env?))
    )

  (define (init-env)
    ($empty-env))

  ; extend-env* :: [symbol] x [ExpVal] x Env -> Env
  (define (extend-env* vars expvals env)
    (if (not (= (length vars) (length expvals)))
        (eopl:error 'extend-tenv "wjh: lengths not matched!")
        (if (null? vars)
            env
            (let [(new-env ($extend-env (car vars) (car expvals) env))]
              (extend-env* (cdr vars) (cdr expvals) new-env)))))
    

  ;; apply-env == look-up-env
  (define (apply-env env var)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'apply-env "Didn't find in env while search : ~s" var))
      ($extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))
      ($extend-env-rec* (fids bvars-s proc-body-s env/without-fids)                       
                        (let [(res (maybe-has-proc? var fids bvars-s proc-body-s))]
                          (if res
                              ($proc-val ($procedure (cadr res) (caddr res) env))
                              (apply-env env/without-fids var))))
      ))

  ; return '(fid bvars body) if exists ; else return #f                                    
  (define (maybe-has-proc? search-f fid-s bvars-s body-s)
    (if (null? fid-s)
        #f
        (if (eqv? search-f (car fid-s))
            (list search-f (car bvars-s) (car body-s))
            (maybe-has-proc? search-f (cdr fid-s) (cdr bvars-s) (cdr body-s)))))

  )
