(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang-in-out.scm")
  (require "utils.scm")
  ; =============================================================== ExpVal
  (define-datatype expval expval?
    ($num-val
     (value number?))
    ($bool-val
     (boolean boolean?))
    ($proc-val 
     (proc Proc?)))

  (define expval->num
    (lambda (v)
      (cases expval v
        ($num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
        ($bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
        ($proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))
  ; =============================================================== Continutaion
  ;; the interpreter is tail-recursive,
  ;; so it really doesn't do anything with the continuation.  
  ;; So all we need is one continuation value.
  
  (define-datatype Continuation Continuation?
    ($end-cont)                     
    )
  ; =============================================================== Proc
  (define-datatype Proc Proc?
    ($procedure
     (vars (list-of symbol?))
     (body TailExp?)
     (env environment?)))
  ; =============================================================== Env
  ;; not precise, but will do.
  (define environment?
    (list-of
     (lambda (p)
       (and 
        (pair? p)
        (or (eqv? (car p) 'let)
            (eqv? (car p) 'letrec))))))
  
  (define (empty-env) '())
  (define empty-env? null?)

  ; extend-env* :: [Symbol] * [Val] -> Env
  (define extend-env*
    (lambda (syms vals old-env)
      (cons (list 'let syms vals)
            old-env)))
  
  (define extend-env-rec*
    (lambda (p-names b-varss p-bodies saved-env)
      (cons (list 'letrec p-names b-varss p-bodies)
            saved-env)))

  (define apply-env
    (lambda (env search-sym)
      (if (null? env) 
          (eopl:error 'apply-env "No binding for ~s" search-sym)
          (let* ((binding (car env))
                 (saved-env (cdr env)))
            (let ((pos (list-index-v2 search-sym (cadr binding))))
              (if pos
                  ; then
                  (case (car binding)
                    ((let) (list-ref (caddr binding) pos))
                    ((letrec) (let ((bvars (caddr binding))
                                    (bodies (cadddr binding)))
                                ($proc-val ($procedure (list-ref bvars pos) (list-ref bodies pos) env)))))
                  ; else
                  (apply-env saved-env search-sym)))))))

  (define init-env 
    (let ((extend-env1
           (lambda (sym val env)
             (extend-env* (list sym) (list val) env))))    
      (lambda ()
        (extend-env1
         'i ($num-val 1)
         (extend-env1
          'v ($num-val 5)
          (extend-env1
           'x ($num-val 10)
           (empty-env)))))))

  )
