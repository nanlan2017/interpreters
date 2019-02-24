(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  (require "utils.scm")
  ;============================================================ Inner Value
  ; expressed values : values of expressions
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p proc?))
    ($mutpair-val
     (p MutPair?))
    )
  ; denoted values : the values bound to variables  e.g 'x 'y 'z
  
  ; //TODO 这层Wrapper是不必要的。直接使用Ref类型就可以了
  ;        要不然还要用  $ref-val  <---> denval->ref  来互相转化！
  
  ;  (define-datatype DenVal DenVal?
  ;    ($ref-val
  ;     (i reference?))
  ;    )
  
  ; denval -> Reference (integer?)
  ;  (define (denval->ref denval)
  ;    (cases DenVal denval
  ;      ($ref-val (r) r)
  ;      (else (eopl:error "Can't get ref-val from DenVal :" denval))
  ;      ))  

  ; expval -> number
  (define (expval->num expval)
    (cases ExpVal expval
      ($num-val (n) n)
      (else (eopl:error "Can't get num-val from ExpVal :" expval))
      ))
  ; expval -> boolean
  (define (expval->bool expval)
    (cases ExpVal expval
      ($bool-val (b) b)
      (else (eopl:error "Can't get bool-val from ExpVal :" expval))
      ))
  ; expval -> Proc
  (define (expval->proc expval)
    (cases ExpVal expval
      ($proc-val (p) p)
      (else (eopl:error "Can't get proc-val from ExpVal :" expval))
      ))
  ; expval -> MutPair
  (define (expval->mutpair expval)
    (cases ExpVal expval
      ($mutpair-val (p) p)
      (else (eopl:error "Can't get proc-val from ExpVal :" expval))
      ))

  ;============================================================ Proc (part 1)
  (define-datatype Procedure proc?
    ($procedure
     (var identifier?)
     (body expression?)
     (env Env?)))
    
  ;============================================================ Env ★ [DenVal] 或 [Ref]
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val Ref?)  ; ★
     (env Env?))
    ($extend-env-rec
     (p-name (list-of identifier?))
     (b-var (list-of identifier?))
     (body (list-of expression?))
     (env Env?))
    )

  (define (init-env)
    ($extend-env 'i (newref ($num-val 1))
                 ($extend-env 'v (newref ($num-val 5))
                              ($extend-env 'x (newref ($num-val 10)) ($empty-env)))))
  
  ; apply-env :: Env -> Symbol -> Ref(ExpVal)
  (define (apply-env env var)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'apply-env "Didn't find in env while search : ~s" var))
      ($extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))
      ($extend-env-rec (pname-s bvar-s pbody-s saved-env)
                       (let [(n (location var pname-s))]
                         (if n
                             (newref ($proc-val (list-ref bvar-s n) (list-ref pbody-s n) env))  ; 查找到这个procedure (ExpVal)
                             (apply-env saved-env var))))
      ))

  ; $extend-env* :: [symbol] x [Val] -> Env
  (define ($extend-env* vars vals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car vals) env))]
          ($extend-env* (cdr vars) (cdr vals) new-env))))
  
  ;============================================================  Store    
  (define STORE 'uninitialized-store)

  (define (empty-store) '())

  (define (get-store) STORE)

  (define (initialize-store!)
    (set! STORE (empty-store)))
  
  ; type Ref = Integer
  (define Ref? integer?)

  ;```````````````````````
  ; newref :: ExpVal -> Ref
  (define (newref val)
    (let [(next-ref (length STORE))]
      (set! STORE (append STORE (list val)))
      next-ref))

  ; deref :: Ref -> ExpVal
  (define (deref i)
    (list-ref STORE i))

  ; setref! :: Ref -> ExpVal -> ()
  (define (setref! idx val)
    (letrec [(setref-inner (lambda (sto i)
                             (cond
                               [(null? sto) (eopl:error "Invalid Reference!")]
                               [(zero? i) (cons val (cdr sto))]
                               [else (cons (car sto) (setref-inner (cdr sto) (- i 1)))])))]
      (set! STORE (setref-inner STORE idx))))

  ;============================================================ Mutable Pairs
  (define-datatype MutPair MutPair?
    ($a-pair
     (left Ref?)
     (right Ref?)))

  ; make-pair :: ExpVal -> ExpVal -> MutPair
  (define (make-pair v1 v2)
    ($a-pair (newref v1) (newref v2)))
  ; left/right : MutPair -> ExpVal
  (define (left pr)
    (cases MutPair pr
      ($a-pair (lr rr)
               (deref lr))))
  (define (right pr)
    (cases MutPair pr
      ($a-pair (lr rr)
               (deref rr))))
  ; setleft/setright : MutPair -> ExpVal -> ()
  (define (setleft pr val)
    (cases MutPair pr
      ($a-pair (lr rr)
               (setref! lr val))))
  (define (setright pr val)
    (cases MutPair pr
      ($a-pair (lr rr)
               (setref! rr val))))

  




























  

  )
