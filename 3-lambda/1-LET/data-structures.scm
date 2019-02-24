(module ds-env-val (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  ;;====================================== Expressed Value | Denoted Value
  (define-datatype ExpVal ExpVal?
    ($ExpVal.num-val
     (num number?))
    ($ExpVal.bool-val
     (v boolean?)))

  ;; expval -> number
  (define (expval->num expv)
    (cases ExpVal expv
      ($ExpVal.num-val (n)
                n)
      ($ExpVal.bool-val (b)
                 (eopl:error "Can't get num-val from boolen ExpVal"))))
  ;; expval -> boolean
  (define (expval->bool expv)
    (cases ExpVal expv
      ($ExpVal.num-val (n)
                (eopl:error "Can't get bool-val from num ExpVal"))
      ($ExpVal.bool-val (b)
                 b)))

  ;;====================================== Env (采用datatype 表示法)
  (define-datatype Env Env?
    ($Env.empty-env)   
    ($Env.extend-env
     (var symbol?)
     (val ExpVal?)  ;注意：这里的val类型为ExpVal
     (env Env?)))

  (define (init-env)
    ($Env.extend-env 'i ($ExpVal.num-val 1)
                 ($Env.extend-env 'v ($ExpVal.num-val 5)
                              ($Env.extend-env 'x ($ExpVal.num-val 10)
                                           ($Env.empty-env)))))

  (define (apply-env env var)
    (cases Env env
      ($Env.empty-env ()
                  (eopl:error 'apply-env "Empty env while search ~s" var))
      ($Env.extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))))
  )
