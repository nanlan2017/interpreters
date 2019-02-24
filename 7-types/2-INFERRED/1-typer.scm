(module typer (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "1-data-structures-static.scm")
  (require "1-unification.scm")  
  ; ========================================================================
  ; 例如：  $an-answer  int->t0  {t0 ~ bool}
  (define-datatype Answer Answer?
    ($an-answer
     (ty Type?)
     (subst substitution?))
    )
  
  (define (typer src)
    (type-to-external-form (type-of-program (scan&parse src))))
  
  (define (type-of-program prog)
    (cases program prog
      (a-program (expr)
                 (cases Answer (typeof expr (init-tenv) (empty-subst))
                   ($an-answer (ty subst)
                               (apply-substs-to-type ty subst))))))
  ; **********************************************************************************************************  
  ; 又是两层架构:
  ;   x  --->  Env { x : ref 1 }           ---> Store {0x1 = 55 }
  ;   x  --->  TEnv{ x :: int -> t0 ...}   ---> Subst {t0 ~ bool}
  ;
  ;
  ;  ████████ For each kind of expression, we recur on the subexpressions, passing along the solution so far in the substitution argument.
  ;  Then we generate the equations for the current expression, according to the specification, and record these in the substitution by calling unify.
  ; **********************************************************************************************************  

  ; typeof 和 unify都会更新 subst
  ; typeof :: Exp * TyEnv * Subst -> Answer (该Exp的类型、迄今unify出的substitions)
  (define (typeof EXP TENV SUBST)
    (cases expression EXP
      (const-exp (n)
                 ($an-answer ($int-type) SUBST))
      (var-exp (var)
               ($an-answer (apply-tenv TENV var) SUBST))
     
      (diff-exp (e1 e2)
                (cases Answer (typeof e1 TENV SUBST)
                  ($an-answer (ty1 subst1)                             
                              (let [(subst1 (█unify ty1 ($int-type) subst1 e1))] ; T(e1) == Int                            
                                (cases Answer (typeof e2 TENV subst1)
                                  ($an-answer (ty2 subst2)                                              
                                              (let [(subst2 (█unify ty2 ($int-type) subst2 e2))] ; T(e2) == Int                                               
                                                ($an-answer ($int-type) subst2))))))))
      (zero?-exp (e1)
                 (cases Answer (typeof e1 TENV SUBST)
                   ($an-answer (ty1 subst1)                               
                               (let [(subst2 (█unify ty1 ($int-type) subst1 EXP))] ; T(e1) == Int
                                 ($an-answer ($bool-type) subst2)))))
      (if-exp (e1 e2 e3)
              (cases Answer (typeof e1 TENV SUBST)
                ($an-answer (ty1 subst)
                            (let [(subst (█unify ty1 ($bool-type) subst e1))] ; T(e1) == Bool
                              (cases Answer (typeof e2 TENV subst)
                                ($an-answer (ty2 subst)
                                            (cases Answer (typeof e3 TENV subst)
                                              ($an-answer (ty3 subst)                                                          
                                                          (let [(subst (█unify ty2 ty3 subst EXP))] ; T(e2) == T(e3)
                                                            ($an-answer ty2 subst))))))))))
                                     
      (let-exp (var e1 body)
               (cases Answer (typeof e1 TENV SUBST)
                 ($an-answer (ty1 subst1)
                             (typeof body ($extend-tenv var ty1 TENV) subst1))))
      
      (proc-exp (var opt-ty body)
                (let [(var-ty (opt-type->type opt-ty))]
                  (cases Answer (typeof body ($extend-tenv var var-ty TENV) SUBST)
                    ($an-answer (ty-body subst)
                                ($an-answer ($proc-type var-ty ty-body) subst)))))
      (call-exp (rator rand)
                (let [(result-type (fresh-tvar-type))]
                  (cases Answer (typeof rator TENV SUBST)
                    ($an-answer (ty-rator subst1)
                                (cases Answer (typeof rand TENV subst1)
                                  ($an-answer (ty-rand subst2)         
                                              (let [(subst3 (█unify ty-rator ($proc-type ty-rand result-type) subst2 EXP))] ; T(rator) = T(rand) -> T(#)
                                                ($an-answer result-type subst3))))))))
      
      (letrec-exp (res-otype pname bvar bvar-otype pbody letrec-body)
                  (let [(res-ty (opt-type->type res-otype))
                        (var-ty (opt-type->type bvar-otype))]                    
                    (let [(tenv-for-letrec-body ($extend-tenv pname ($proc-type var-ty res-ty) TENV))]
                      (cases Answer (typeof pbody ($extend-tenv bvar var-ty tenv-for-letrec-body) SUBST)
                        ($an-answer (pbody-ty subst)
                                    (let ((subst (█unify pbody-ty res-ty subst pbody))) ; T(p-body) = T(result-type)
                                      (typeof letrec-body tenv-for-letrec-body subst)))))))
      ))
                  
      


  )
