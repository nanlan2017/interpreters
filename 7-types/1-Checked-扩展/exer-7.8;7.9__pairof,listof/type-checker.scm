(module type-checker (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures-static.scm")
  ; =======================================================================
  (define (typeof-program prog)
    (cases program prog
      ($a-program (expr)
                  (typeof expr (init-tenv)))))

  ; typeof :: Exp * tenv -> type
  (define (typeof exp tenv)
    (cases expression exp
      ($const-exp (n)
                  ($int-type))
      ($var-exp (x)
                (apply-tenv tenv x))
      ($diff-exp (e1 e2)
                 (let [(ty1 (typeof e1 tenv))
                       (ty2 (typeof e2 tenv))]
                   (check-equal-type! ty1 ($int-type) e1)
                   (check-equal-type! ty2 ($int-type) e1)
                   ($int-type)))
      ($zero?-exp (e1)
                  (let [(ty1 (typeof e1 tenv))]
                    (check-equal-type! ty1 ($int-type) e1)
                    ($bool-type)))
      ($if-exp (e1 e2 e3)
               (begin 
                 (let [(ty1 (typeof e1 tenv))]
                   (check-equal-type! ty1 ($bool-type) e1))
                 (let [(ty2 (typeof e2 tenv))
                       (ty3 (typeof e3 tenv))]                 
                   (check-equal-type! ty2 ty3 exp)
                   ty2)))
      ($let-exp (var e1 body)
                (let [(ty1 (typeof e1 tenv))]
                  (typeof body ($extend-tenv var ty1 tenv))))
      ;; proc
      ($proc-exp (var ty body)
                 (let [(res-type (typeof body ($extend-tenv var ty tenv)))]
                   ($proc-type ty res-type)))
      ($call-exp (rator rand)
                 (let ((rator-type (typeof rator tenv))
                       (rand-type (typeof rand tenv)))
                   (cases Type rator-type
                     ($proc-type (arg-type result-type)
                                 (begin
                                   (check-equal-type! arg-type rand-type rand)
                                   result-type))
                     (else (report-rator-not-a-proc-type rator-type rator)))))                               
      ;; letrec
      ($letrec-exp (p-res-type pid b-var b-var-type p-body letrec-body)
                   (let* [(new-tenv ($extend-tenv pid ($proc-type b-var-type p-res-type) tenv))
                          (p-body-type (typeof p-body ($extend-tenv b-var b-var-type new-tenv)))]
                     (check-equal-type! p-res-type p-body-type p-body)
                     (typeof letrec-body new-tenv)))
      ; ---------------------------------
      ($newpair-exp (e1 e2)
                    ($pair-type (typeof e1 tenv) (typeof e2 tenv)))
      
      ($unpair-exp (var1 var2 e1 body)
                   (let [(t1 (typeof e1 tenv))]
                     (cases Type t1
                       ($pair-type (ty1 ty2)
                                   (typeof body (extend-tenv* (list var1 var2) (list ty1 ty2) tenv)))
                       (else (eopl:error 'unpair-exp "not a pair type")))))
      ; ---------------------------------
      ($list-exp (e1 exps)
                 ; TODO : check
                 ($list-type (typeof e1 tenv)))
      ($cons-exp (e1 exp)
                 ; TODO : check
                 ($list-type (typeof e1 tenv)))
      ($null?-exp (exp)
                  ; TODO : check 
                  ($bool-type))
      ($emptylist-exp (ty)
                      ($list-type ty))
                  
      ))


  (define (check src)
    (type-to-external-form (typeof-program (scan&parse src))))

  )
