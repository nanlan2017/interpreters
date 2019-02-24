(module typer (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "1-data-structures-static.scm")
  (require "1-unification.scm")  
  ; ========================================================================
  ; 例如：  $an-answer  int->t0  {t0 ~ bool}
  ;  (define-datatype Answer Answer?
  ;    ($an-answer
  ;     (ty Type?)
  ;     (subst substitution?))
  ;    )

  (define-datatype EquationSet EquationSet?
    ($empty-eqset)
    (█extend-eqset
     (tvar Type?)
     (ty Type?)
     (eqset EquationSet?)))
  
  (define-datatype Problem Problem?
    ($a-problem
     (ty Type?)
     (equations EquationSet?)))
  ; ------------------------------------------------------------------------
  (define (typer src)
    (type-to-external-form (type-of-program (scan&parse src))))
  
  (define (type-of-program prog)
    (cases program prog
      (a-program (expr)
                 (cases Problem (typeof expr (init-tenv) ($empty-eqset))
                   ($a-problem (ty eqset)
                               (let [(substs (solve eqset (empty-subst)))]
                                 (apply-substs-to-type ty substs)))))))
                               
  (define (listfy-eqset eqset)
    (cases EquationSet eqset
      ($empty-eqset ()
                    '())
      (█extend-eqset (tvar ty saved-eqset)
                     (cons (cons (type-to-external-form tvar)
                                 (type-to-external-form ty))
                           (listfy-eqset saved-eqset)))))

  (define (listfy-substs substs)
    (if (null? substs)
        '()
        (let [(pr (car substs))
              (left-substs (cdr substs))]
          (cons (cons (type-to-external-form (car pr))
                      (type-to-external-form (cdr pr)))
                (listfy-substs left-substs)))))
  
  ;  :: EquationSet -> Substitution 
  (define (solve eqset substs)
    (eopl:printf "Solving equations...~n~s~n" (listfy-eqset eqset))    
    (eopl:printf "~n")
    
    (cases EquationSet eqset
      ($empty-eqset ()
                    substs)
      (█extend-eqset (tvar ty saved-eqset)
                     (let [(tmp-substs (solve saved-eqset substs))]
                       (eopl:printf "Substitutions...~n~s~n" (listfy-substs tmp-substs))
                       (█unify tvar ty tmp-substs 'no-exp)))))

  ; =======================================================================
  ; typeof 和 $extend-eqset 都会更新 EqSet
  ; typeof :: Exp * TyEnv * EquationSet -> Problem
  ; EqSet只会被不断加入新的Equation 
  (define (typeof EXP TENV eqset)
    (cases expression EXP
      (const-exp (n)
                 ($a-problem ($int-type) eqset))
      (var-exp (var)
               ($a-problem (apply-tenv TENV var) eqset))
     
      (diff-exp (e1 e2)
                (cases Problem (typeof e1 TENV eqset)
                  ($a-problem (ty1 eqset1)                             
                              (let [(new-eqset (█extend-eqset ty1 ($int-type) eqset1))]                        
                                (cases Problem (typeof e2 TENV new-eqset)
                                  ($a-problem (ty2 eqset2)                                              
                                              (let [(new-eqset (█extend-eqset ty2 ($int-type) eqset2))]                                            
                                                ($a-problem ($int-type) new-eqset))))))))
      (zero?-exp (e1)
                 (cases Problem (typeof e1 TENV eqset)
                   ($a-problem (ty1 eqset1)                               
                               (let [(eqset2 (█extend-eqset ty1 ($int-type) eqset1))] 
                                 ($a-problem ($bool-type) eqset2)))))
      (if-exp (e1 e2 e3)
              (cases Problem (typeof e1 TENV eqset)
                ($a-problem (ty1 eqset1)
                            (let [(new-eqset (█extend-eqset ty1 ($bool-type) eqset1))] 
                              (cases Problem (typeof e2 TENV new-eqset)
                                ($a-problem (ty2 eqset)
                                            (cases Problem (typeof e3 TENV eqset)
                                              ($a-problem (ty3 eqset)                                                          
                                                          (let [(eqset (█extend-eqset ty2 ty3 eqset))]
                                                            ($a-problem ty2 eqset))))))))))
                                     
      (let-exp (var e1 body)
               (cases Problem (typeof e1 TENV eqset)
                 ($a-problem (ty1 eqset1)
                             (typeof body ($extend-tenv var ty1 TENV) eqset1))))
      
      (proc-exp (var opt-ty body)
                (let [(var-ty (opt-type->type opt-ty))]
                  (cases Problem (typeof body ($extend-tenv var var-ty TENV) eqset)
                    ($a-problem (ty-body eqset)
                                ($a-problem ($proc-type var-ty ty-body) eqset)))))
      (call-exp (rator rand)
                (let [(result-type (fresh-tvar-type))]
                  (cases Problem (typeof rator TENV eqset)
                    ($a-problem (ty-rator eqset1)
                                (cases Problem (typeof rand TENV eqset1)
                                  ($a-problem (ty-rand eqset2)         
                                              (let [(eqset3 (█extend-eqset ty-rator ($proc-type ty-rand result-type) eqset2))] 
                                                ($a-problem result-type eqset3))))))))
      
      (letrec-exp (res-otype pname bvar bvar-otype pbody letrec-body)
                  (let [(res-ty (opt-type->type res-otype))
                        (var-ty (opt-type->type bvar-otype))]                    
                    (let [(tenv-for-letrec-body ($extend-tenv pname ($proc-type var-ty res-ty) TENV))]
                      (cases Problem (typeof pbody ($extend-tenv bvar var-ty tenv-for-letrec-body) eqset)
                        ($a-problem (pbody-ty eqset1)
                                    (let ((eqset2 (█extend-eqset pbody-ty res-ty eqset1))) 
                                      (typeof letrec-body tenv-for-letrec-body eqset2)))))))
      ))
                  
      


  )
