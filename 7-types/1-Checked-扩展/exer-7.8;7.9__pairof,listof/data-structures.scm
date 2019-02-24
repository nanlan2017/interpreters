(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  ;;====================================== Expressed Value | Denoted Value
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p Proc?))
    ($pair-val
     (v1 ExpVal?)
     (v2 ExpVal?))
    ($list-val
     (vals (list-of ExpVal?)))
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

  (define expval->pair-left
    (lambda (v)
      (cases ExpVal v
        ($pair-val (v1 v2) v1)
        (else (expval-extractor-error 'pair v)))))
  
  (define expval->pair-right
    (lambda (v)
      (cases ExpVal v
        ($pair-val (v1 v2) v2)
        (else (expval-extractor-error 'pair v)))))
  
  (define expval->list
    (lambda (v)
      (cases ExpVal v
        ($list-val (vals) vals)
        (else (expval-extractor-error 'proc v)))))
  
  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))
  ;;====================================== Proc (抽象类型)
  (define-datatype Proc Proc?
    ($procedure
     (var identifier?)
     (body expression?)
     (env Env?)))   
  ;;====================================== Env (采用datatype 表示法)
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?)
     (env Env?))
    ($extend-env-rec
     (p-name identifier?)
     (b-var identifier?)
     (body expression?)
     (env Env?))
    )

  (define (init-env)
    ($empty-env))

  ; extend-env* :: [symbol] x [ExpVal] x Env -> Env
  (define (extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          (extend-env* (cdr vars) (cdr expvals) new-env))))

  ;; apply-env == look-up-env
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
      ))

  )
