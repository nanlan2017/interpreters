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
                   (█check-equal-type! ty1 ($int-type) e1)
                   (█check-equal-type! ty2 ($int-type) e1)
                   ($int-type)))
      ($zero?-exp (e1)
                  (let [(ty1 (typeof e1 tenv))]
                    (█check-equal-type! ty1 ($int-type) e1)
                    ($bool-type)))
      ($if-exp (e1 e2 e3)
               (let ((ty1 (typeof e1 tenv))
                     (ty2 (typeof e2 tenv))
                     (ty3 (typeof e3 tenv)))
                 (█check-equal-type! ty1 ($bool-type) e1)
                 (█check-equal-type! ty2 ty3 exp)
                 ty2))
      ($let-exp (vars es body)
                (let [(tys (map (lambda (e) (typeof e tenv)) es))]
                  (typeof body (extend-tenv* vars tys tenv))))
      ;; proc
      ($proc-exp (vars tys body)
                 (let [(res-type (typeof body (extend-tenv* vars tys tenv)))]
                   ($proc-type tys res-type)))
      
      ($call-exp (rator rands)
                 (let ((rator-type (typeof rator tenv))
                       (rand-types (map (lambda (e) (typeof e tenv)) rands)))
                   (cases Type rator-type
                     ($proc-type (arg-ty-s result-type)
                                 (begin
                                   (for-each █check-equal-type! arg-ty-s rand-types rands)
                                   result-type))
                     (else (report-rator-not-a-proc-type rator-type rator)))))                               
      ;; letrec
      ($letrec-exp (p-restype-s pid-s bvars-s bvar-tys-s p-body-s letrec-body)
                   (let* [(tenv/letrec-body (extend-tenv* pid-s
                                                  (map $proc-type bvar-tys-s p-restype-s)
                                                  tenv))
                          (p-body-type-s (map (lambda (body bvars bvars-tys)
                                                (typeof body (extend-tenv* bvars bvars-tys tenv/letrec-body)))
                                              p-body-s
                                              bvars-s
                                              bvar-tys-s))]                    
                     (for-each █check-equal-type! p-restype-s p-body-type-s p-body-s)
                     (typeof letrec-body tenv/letrec-body)))
                  
      ))


  (define (check src)
    (type-to-external-form (typeof-program (scan&parse src))))

  )
