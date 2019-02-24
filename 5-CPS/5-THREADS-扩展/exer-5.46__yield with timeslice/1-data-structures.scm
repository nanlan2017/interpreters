(module data-structures (lib "eopl.ss" "eopl")
  (require "0-lang.scm")
  (require "0-store.scm")
  (provide (all-defined-out))

  (define identifier? symbol?)
 
  ;; used by begin-exp
  (define fresh-identifier
    (let ((sn 0))
      (lambda (identifier)  
        (set! sn (+ sn 1))
        (string->symbol (string-append (symbol->string identifier) "%" (number->string sn))))))
  ; ======================================================================== ExpVal
  (define-datatype ExpVal ExpVal?
    ($stub
     (var symbol?))  ; 
    ($num-val
     (value number?))
    ($bool-val
     (boolean boolean?))
    ($proc-val 
     (proc Proc?))
    ($list-val
     (lst (list-of ExpVal?)))
    ($mutex-val
     (mutex Mutex?))
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

  (define expval->list
    (lambda (v)
      (cases ExpVal v
        ($list-val (lst) lst)
        (else (expval-extractor-error 'list v)))))

  (define expval->mutex
    (lambda (v)
      (cases ExpVal v
        ($mutex-val (l) l)
        (else (expval-extractor-error 'mutex v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))
  
  ; ================================================================ Env [symbol ~ Ref]
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val reference?)
     (env Env?))
    ($extend-env-rec*
     (fids (list-of identifier?))
     (bvars (list-of identifier?))
     (body-s (list-of expression?))
     (env Env?))     
    )

  (define (init-env)
    ($empty-env))

  (define (apply-env env var)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'apply-env "Empty env while search ~s" var))
      ($extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))
      ($extend-env-rec* (fids bvars proc-body-s env/without-fids)                       
                        (let [(res (maybe-has-proc? var fids bvars proc-body-s))]
                          (if res
                              (newref ($proc-val ($procedure (cadr res) (caddr res) env)))
                              (apply-env env/without-fids var))))
      ))
  
  ; return '(fid bvars body) if exists ; else return #f                                    
  (define (maybe-has-proc? search-f fid-s bvar-s body-s)
    (if (null? fid-s)
        #f
        (if (eqv? search-f (car fid-s))
            (list search-f (car bvar-s) (car body-s))
            (maybe-has-proc? search-f (cdr fid-s) (cdr bvar-s) (cdr body-s)))))
                                      

  ; extend-env* :: [symbol] x [ExpVal] -> Env
  (define (extend-env* vars vals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car vals) env))]
          (extend-env* (cdr vars) (cdr vals) new-env))))  
  

  
  ; ======================================================================== procedures
  (define-datatype Proc Proc?
    ($procedure
     (bvar symbol?)
     (body expression?)
     (env Env?)))

  ; ======================================================================== Thread, Mutex
  (define-datatype Thread Thread?
    ($a-thread
     (thunk procedure?)
     (time-remaining integer?)))

  (define (get-thunk-of-thread th)
    (cases Thread th
      ($a-thread (thk time)
                 thk)))
  (define (get-time-remaining-of-thread th)
    (cases Thread th
      ($a-thread (thk time)
                 time)))
  ;--------------------------------------
  ; “锁” 
  ; A mutex may either be open or closed.
  ; It also contains a queue of threads that are waiting for the mutex to become open
  (define-datatype Mutex Mutex?
    ($a-mutex
     (ref@closed?    reference?)    ; ref to bool
     (ref@wait-queue reference?)))  ; ref to (listof thread)

  )
