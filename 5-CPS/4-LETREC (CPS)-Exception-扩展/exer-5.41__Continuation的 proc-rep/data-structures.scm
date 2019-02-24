(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  
  (define identifier? symbol?)
  ;;====================================== Expressed Value | Denoted Value
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p Proc?))
    ($list-val
     (vals (list-of ExpVal?)))
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
  ;; expval -> List
  (define (expval->list expval)
    (cases ExpVal expval
      ($list-val (vs) vs)
      (else (eopl:error "Can't get list-val from ExpVal :" expval))
      ))
  ;;====================================== Proc (采用datatype 表示法)
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

  (define (init-env)
    ($empty-env))

  ; extend-env* :: [symbol] x [ExpVal] x Env -> Env
  (define (extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          (extend-env* (cdr vars) (cdr expvals) new-env))))
  
  ;  ;;====================================== Continuation
  ;  (define-datatype Continuation Continuation?
  ;    ($end-cont)
  ;
  ;    ; Exception
  ;    ($try-cont
  ;     (cvar identifier?)
  ;     (contvar identifier?)
  ;     (handler-exp expression?)
  ;     (env Env?)
  ;     (cont Continuation?))
  ;    
  ;    ($raise1-cont
  ;     (cont Continuation?))
  ;    ;````````````````````````````````
  ;    ; unary : zero? | null? car cdr
  ;    ($unary-arg-cont
  ;     (op unary-op?)
  ;     (cont Continuation?))
  ;    
  ;    ; if-exp
  ;    ($if-test-cont
  ;     (then-exp expression?)
  ;     (else-exp expression?)
  ;     (env Env?)
  ;     (cont Continuation?))
  ;    
  ;    ; diff-exp
  ;    ($diff1-cont
  ;     (e2 expression?)
  ;     (env Env?)
  ;     (cont Continuation?))
  ;    ($diff2-cont
  ;     (v1 ExpVal?)
  ;     (cont Continuation?))
  ;    
  ;    ; call-exp
  ;    ($rator-cont
  ;     (rand-exp expression?)
  ;     (env Env?)
  ;     (cont Continuation?))
  ;    ($rand-cont
  ;     (ratorval ExpVal?)   ; (c x) 可能是函数应用，也可能是contination应用
  ;     (cont Continuation?))    
  ;    )






  
  )
