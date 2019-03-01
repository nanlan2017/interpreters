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
  ;; expval -> [ExpVal]
  (define (expval->list expval)
    (cases ExpVal expval
      ($list-val (vals) vals)
      (else (eopl:error "Can't get list-val from ExpVal :" expval))
      ))
  ;; expval -> Proc
  (define (expval->proc expval)
    (cases ExpVal expval
      ($proc-val (p) p)
      (else (eopl:error "Can't get proc-val from ExpVal :" expval))
      ))
  ;;====================================== Proc (抽象类型)
  (define-datatype Proc Proc?
    ($procedure
     (body expression?)
     (env env?)))
  
  ;;====================================== SEnv  [i,v,x]
  ;; SEnv :: [symbol]
  ;; LexAddress :: Int
  (define (empty-senv)
    '())
  
  (define (extend-senv var senv)
    (cons var senv))

  (define extend-senv* append)

  ;; == list-index
  (define (apply-senv senv var)
    (cond
      [(null? senv) (eopl:error "Unbounded var :~s" var)]
      [(eqv? var (car senv)) 0]
      [else (+ 1 (apply-senv (cdr senv) var))]))

  (define (init-senv)
    (extend-senv 'i
                 (extend-senv 'v
                              (extend-senv 'x (empty-senv)))))
  ;;====================================== Env   [1,5,10]
  (define env? (list-of ExpVal?))
  
  (define (empty-env)
    '())

  (define (extend-env val env)
    (cons val env))

  (define extend-env* append)

  ;; == list-ref
  (define (apply-env env idx)
    (let ([size (length env)])
      (cond
        [(> idx size) (eopl:error "idx > size")]
        [(= 0 idx) (car env)]
        [else (apply-env (cdr env) (- idx 1))])))

  (define (init-env)
    (list ($num-val 1) ($num-val 5) ($num-val 10)))
    
  )
