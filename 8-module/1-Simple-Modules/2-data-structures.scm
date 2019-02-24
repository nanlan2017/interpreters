(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "0-lang.scm")

  ;;============================================================= Expressed Value
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p Proc?))
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
  ;;============================================================= Proc
  (define-datatype Proc Proc?
    ($procedure
     (var identifier?)
     (body Expression?)
     (env Env?)))    
  ;;============================================================= Env (采用datatype 表示法)
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?)
     (env Env?))
    ($extend-env-rec
     (p-name identifier?)
     (b-var identifier?)
     (body Expression?)
     (env Env?))
    ; module
    ($extend-env-with-module
     (mod-id symbol?)
     (mod-val TypedModule?)
     (env Env?))
    )

  (define (get-nested-env env)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'get-nested-env "No nested env for Empty-env!"))
      ($extend-env (saved-var saved-val saved-env)
                   saved-env)
      ($extend-env-rec (p-name b-var p-body saved-env)
                       saved-env)
      ($extend-env-with-module (mod-id mod-val saved-env)
                               saved-env)
      ))

  (define (init-env)
    ($empty-env))


  (define (extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          (extend-env* (cdr vars) (cdr expvals) new-env))))


  (define (apply-env env var)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'apply-env "Didn't find in env while search : ~s" var))
      ($extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))
      ($extend-env-rec (p-name b-var p-body saved-env)
                       (if (eqv? var p-name)
                           ($proc-val ($procedure b-var p-body env))
                           (apply-env saved-env var)))
      ($extend-env-with-module (mod-id mod-val saved-env)
                               (apply-env saved-env var))
      ))
  ;;============================================================= TypedModule = {Env == let* a ;b;c }
  (define-datatype TypedModule TypedModule?
    ($a-simple-module
     (bindings Env?))
    )

  (define (lookup-qualified-var-in-env mod-name var-name env)
    (let [(mod-val (lookup-module-in-env mod-name env))]
      (cases TypedModule mod-val
        ($a-simple-module (bindings)
                          (apply-env bindings var-name)))))  

  (define (lookup-module-in-env mod-name env)
    (cases Env env
      ($extend-env-with-module (mod-id mod-val saved-env)
                               (if (eqv? mod-name mod-id)
                                   mod-val
                                   (lookup-module-in-env mod-name saved-env)))
      (else (lookup-module-in-env mod-name (get-nested-env env)))))

  )
