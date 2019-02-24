(module data-structures-static (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  ;;======================================= tenv (symbol <-> type)
  (define-datatype TEnv TEnv?
    ($empty-tenv)
    ($extend-tenv
     (var symbol?)
     (ty Type?)
     (tenv TEnv?))
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
                   (eopl:error 'apply-tenv "Didn't find in type-env while search : ~s" var))
      ($extend-tenv (saved-var saved-ty saved-tenv)
                    (if (equal? var saved-var)
                        saved-ty
                        (apply-tenv saved-tenv var)))))
  ; =========================================================================
  ; check-equal-type! :: Type * Type * Exp -> ()
  (define (check-equal-type! ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp)
        #t))

  (define (report-unequal-types ty1 ty2 exp)
    (eopl:error 'check-equal-type! "Types didn’t match: ~s != ~a in~%~a"
                (type-to-external-form ty1) (type-to-external-form ty2) exp))

  (define (report-rator-not-a-proc-type rator-type rator)
    (eopl:error 'report-rator-not-a-proc-type "Actual Rator type is ~s in ~s , that's illegal!~n" rator-type rator))

  (define (type-to-external-form ty)
    (cases Type ty
      ($int-type ()
                 'int)
      ($bool-type ()
                  'bool)
      ($proc-type (arg-ty res-ty)
                  (list (type-to-external-form arg-ty) '-> (type-to-external-form res-ty)))
      ; --------------
      ($pair-type (ty1 ty2)  ; <int,bool>
                  (list '< (type-to-external-form ty1) ', (type-to-external-form ty2) '>))
      ($list-type (ty)     ; [int]
                  (list '【 (type-to-external-form ty) '】 ))
      ))

  )
