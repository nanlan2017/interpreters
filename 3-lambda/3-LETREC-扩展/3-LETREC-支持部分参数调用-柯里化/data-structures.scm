(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  (require "utils.scm")
  ;;====================================== Expressed Value | Denoted Value
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p proc?))
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
  ;;====================================== Proc (抽象类型)
  ; proc? :: SchemeVal -> Bool
  (define (proc? v)
    (procedure? v)) 
    
  ;;====================================== Env (采用datatype 表示法)
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?)  ;注意：这里的val类型为ExpVal
     (env Env?))
    ; rec
    ($extend-env-rec
     (p-name identifier?)
     (b-var identifier?)
     (body expression?)
     (env Env?))
    )

  (define (init-env)
    ($extend-env 'i ($num-val 1)
                 ($extend-env 'v ($num-val 5)
                              ($extend-env 'x ($num-val 10)
                                           ($empty-env)))))

  ; $extend-env* :: [symbol] x [ExpVal] -> Env
  (define ($extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          ($extend-env* (cdr vars) (cdr expvals) new-env))))

  )
