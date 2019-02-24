(module unification (lib "eopl.ss" "eopl")
  (require "0-lang.scm")
  (require "1-data-structures-static.scm")
  (require "utils.scm")

  (provide substitution?
           empty-subst
           apply-substs-to-type
           █unify)
  ; **********************************************************************************************
  ; Briefly, unification is the process of finding a substitution that makes two given terms equal.
  ; type inference is done by applying unification to type expressions (e.g. 'a -> 'b -> 'a) 


  ; ███████ 不是先一下子把所有Equations联立出来，而是一条一条建立、unify到当前已有的substitutions中！
  ; 主要就3个动作：  apply-one-subst ,  apply-substs,  unify
  
  ;  (define-datatype Subst Subst?
  ;    ($a-subst
  ;     (tvar Type?)
  ;     (ty Type?)))
  ; **********************************************************************************************

  ; substitution  (如同env: t_i <-> Type)
  (define (empty-subst)
    '())

  (define substitution? 
    (list-of (pair-of tvar-type? Type?)))

  ;  extend-subst :: S * tv * t -> S[tv=t]   // S[..] 仍然是一个被加入equal pairs的 substitutions
  (define (extend-subst subst tvar ty)
    (cons (cons tvar ty)
          (map (lambda (pr) (let [(old-lhs (car pr))
                                  (old-rhs (cdr pr))]
                              (cons old-lhs (apply-one-subst old-rhs tvar ty))))
               subst)))

  ; 辅助 @ extend-subst
  ; 例子:t2 ~ bool->t3  [t3 = int]   ====>   t2 ~ bool->int ;  t3 ~ int
  ; 
  ; apply-one-subst :: Type * Tvar * Type -> Type   
  (define (apply-one-subst ty0 tvar ty1)
    (cases Type ty0
      ($int-type ()
                 ($int-type))
      ($bool-type ()
                  ($bool-type))
      ($proc-type (arg-type result-type)
                  ($proc-type
                   (apply-one-subst arg-type tvar ty1)
                   (apply-one-subst result-type tvar ty1)))
      ($tvar-type (sn)
                  (if (equal? ty0 tvar)
                      ty1
                      ty0))
      ))

  ; apply-substs-to-type :: Type * S -> tS     // tS 仍然是一个被替换过的 type-exp
  (define (apply-substs-to-type ty substs)
    (cases Type ty
      ($int-type ()
                 ($int-type))
      ($bool-type ()
                  ($bool-type))
      ($proc-type (t1 t2)
                  ($proc-type (apply-substs-to-type t1 substs)
                              (apply-substs-to-type t2 substs)))
      ($tvar-type (sn)
                  (let ((tmp (assoc ty substs)))
                    (if tmp
                        (cdr tmp)
                        ty)))
      ))

  ; 更新subst : 使得 Type1 = Type2 这条约束equation成立。
  ; 
  ; unifier :: Type1 * Type2 * Subst --> Subst 
  (define (█unify ty1 ty2 subst exp)
    (let ((ty1 (apply-substs-to-type ty1 subst))
          (ty2 (apply-substs-to-type ty2 subst)))
      (cond
        ; 逻辑过程: P 263
        ((equal? ty1 ty2) subst)
        ; t0 == ...
        ((tvar-type? ty1) (if (no-occurrence? ty1 ty2)
                              (extend-subst subst ty1 ty2)
                              (report-no-occurrence-violation ty1 ty2 exp)))
        ; ... == t0
        ((tvar-type? ty2) (if (no-occurrence? ty2 ty1)
                              (extend-subst subst ty2 ty1)
                              (report-no-occurrence-violation ty2 ty1 exp)))
        ; (t1->t2) == (t3->t4)  
        ((and (proc-type? ty1) (proc-type? ty2))
         (let ((subst1 (█unify (proc-type->arg-type ty1)
                               (proc-type->arg-type ty2)
                               subst
                               exp)))
           (let ((subst2 (█unify (proc-type->result-type ty1)
                                 (proc-type->result-type ty2)
                                 subst1
                                 exp)))
             subst2)))
        (else (report-unification-failure ty1 ty2 exp)))))
  
  ;===================================================================================================
  (define report-unification-failure
    (lambda (ty1 ty2 exp) 
      (eopl:error 'unification-failure
                  "Type mismatch: ~s doesn't match ~s in ~s~%"
                  (type-to-external-form ty1)
                  (type-to-external-form ty2)
                  exp)))

  (define report-no-occurrence-violation
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-no-occurence!
                  "Can't unify: type variable ~s occurs in type ~s in expression ~s~%" 
                  (type-to-external-form ty1)
                  (type-to-external-form ty2)
                  exp)))
  
  
  ;; usage: Is there an occurrence of tvar in ty?
  ;; 举例   t0 ~  int -> t0  就 occur了！
  ;
  ;; no-occurrence? : Tvar * Type -> Bool
  (define (no-occurrence? tvar tyexp)
    (cases Type tyexp
      ($int-type () #t)
      ($bool-type () #t)
      ($proc-type (arg-type result-type)
                  (and
                   (no-occurrence? tvar arg-type)
                   (no-occurrence? tvar result-type)))
      ($tvar-type (serial-number) (not (equal? tvar tyexp)))))
  



  )
