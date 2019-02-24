(module data-structures-static (lib "eopl.ss" "eopl")
  
  (require "0-lang.scm")

  (provide (all-defined-out))
  ;;=========================================================== tenv (symbol <-> type)
  (define-datatype TEnv TEnv?
    ($empty-tenv)
    ($extend-tenv
     (var symbol?)
     (ty Type?)
     (tyenv TEnv?))
    )

  (define (init-tenv)
    ($empty-tenv))
  
  (define (extend-tenv* vars tys env)
    (if (not (= (length vars) (length tys)))
        (eopl:error 'extend-tenv "wjh: lengths not matched!")
        (if (null? vars)
            env
            (let [(new-tenv ($extend-tenv (car vars) (car tys) env))]
              (extend-tenv* (cdr vars) (cdr tys) new-tenv)))))
  
  (define (apply-tenv tenv var)
    (cases TEnv tenv
      ($empty-tenv ()
                   (eopl:error 'apply-tyenv "Didn't find in type-env while search : ~s" var))
      ($extend-tenv (saved-var saved-ty saved-tenv)
                    (if (equal? var saved-var)
                        saved-ty
                        (apply-tenv saved-tenv var)))))
  ; ===============================================================
  (define atomic-type?
    (lambda (ty)
      (cases Type ty
        ($proc-type (ty1 ty2) #f)
        ($tvar-type (sn) #f)
        (else #t))))

  (define proc-type?
    (lambda (ty)
      (cases Type ty
        ($proc-type (t1 t2) #t)
        (else #f))))

  (define tvar-type?
    (lambda (ty)
      (cases Type ty
        ($tvar-type (serial-number) #t)
        (else #f))))

  (define proc-type->arg-type
    (lambda (ty)
      (cases Type ty
        ($proc-type (arg-type result-type) arg-type)
        (else (eopl:error 'proc-type->arg-type "Not a proc type: ~s" ty)))))

  (define proc-type->result-type
    (lambda (ty)
      (cases Type ty
        ($proc-type (arg-type result-type) result-type)
        (else (eopl:error 'proc-type->result-types  "Not a proc type: ~s" ty)))))

  (define (opt-type->type otype)
    (cases OptionalType otype
      ($no-type ()
                (fresh-tvar-type))
      ($a-param-type (ty)
               ty)
      ($a-ret-type (ty)
                      ty)
      ))
  
  ; ===============================================================
  ;; type-to-external-form : Type -> List of symbols
  (define type-to-external-form
    (lambda (ty)
      (cases Type ty
        ($int-type () 'int)
        ($bool-type () 'bool)
        ($proc-type (arg-type result-type)
                    (list (type-to-external-form arg-type) '-> (type-to-external-form result-type)))
        ($tvar-type (sn)
                    (string->symbol (string-append "t" (number->string sn))))
        )))

  (define fresh-tvar-type
    (let [(sn 0)]
      (lambda ()
        (set! sn (+ 1 sn))
        ($tvar-type sn))))

  )
