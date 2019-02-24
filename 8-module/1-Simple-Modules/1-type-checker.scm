(module type-checker (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "1-data-structures-static.scm")
  (require "expand-type.scm")

  ; ===================================================================================================
  ; ★ [ModuleDefinition] * TEnv -> TEnv
  (define (add-module-defns-to-tenv defns tenv)
    (if (null? defns)
        tenv
        (cases ModuleDefinition (car defns)
          ($a-module-definition (m-name expected-iface m-body)
                                (let ((actual-iface (interface-of m-body tenv)))
                                  (if (<:-iface actual-iface expected-iface tenv)
                                      (let ((new-tenv ($extend-tenv-with-module m-name expected-iface tenv)))
                                        (add-module-defns-to-tenv (cdr defns) new-tenv))
                                      (report-module-doesnt-satisfy-iface m-name expected-iface actual-iface)))))))
  
  ; interface-of :: ModuleBody * TEnv -> ModuleInterface
  (define (interface-of m-body tenv)
    (cases ModuleBody m-body
      ($a-module-body (var-defn-s)
                      ($a-simple-interface (defns-to-decls var-defn-s tenv)))))

  ; let* scoping!
  ; defns-to-decls : [Defn] × Tenv → [Decl]
  (define (defns-to-decls defns tenv)
    (if (null? defns)
        '()
        (cases VarDefinition (car defns)
          ($a-var-definition (var-name exp)
                             (let ((ty (typeof exp tenv)))
                               (cons ($a-var-declaration var-name ty)
                                     (defns-to-decls (cdr defns) ($extend-tenv var-name ty tenv))))))))
  
  ; <:-iface :: Interface * Interface * TEnv -> Bool
  (define (<:-iface iface1 iface2 tenv)
    (cases SimpleInterface iface1
      ($a-simple-interface (var-decl-s1)
                           (cases SimpleInterface iface2
                             ($a-simple-interface (var-decl-s2)
                                                  (<:-decls var-decl-s1 var-decl-s2 tenv))))))

  ; [a:Int ; b:Int]   <:   [a:Int]
  (define (<:-decls decls1 decls2 tenv)
    (cond
      ((null? decls2) #t)
      ((null? decls1) #f)
      (else (let ((name1 (decl->name (car decls1)))
                  (name2 (decl->name (car decls2))))
              (if (eqv? name1 name2)
                  (and (equal? (decl->type (car decls1)) (decl->type (car decls2)))
                       (<:-decls (cdr decls1) (cdr decls2) tenv))
                  (<:-decls (cdr decls1) decls2 tenv)))))) 

  
  (define (report-module-doesnt-satisfy-iface m-name expected-type actual-type)
    ; (eopl:printf  (list 'error-in-defn-of-module: m-name 'expected-type: expected-type 'actual-type: actual-type))
    (eopl:error "Interface vs Implemention : module ~s : body doesn't implent interface properly.~n" m-name)
    )

  ; =========================================================================================================
  (define (check src)
    (type-to-external-form (typeof-program (scan&parse src))))
  
  (define (typeof-program pgm)
    (cases Program pgm
      ($a-program (mod-defn-s exp)
                  (typeof exp (add-module-defns-to-tenv mod-defn-s (init-tenv))))))

  (define (typeof exp tenv)
    (cases Expression exp
      ($const-exp (num)
                  ($int-type))
      ($var-exp (var)
                (apply-tenv tenv var))
        
      ($diff-exp (exp1 exp2)
                 (let ((type1 (typeof exp1 tenv))
                       (type2 (typeof exp2 tenv)))
                   (check-equal-type! type1 ($int-type) exp1)
                   (check-equal-type! type2 ($int-type) exp2)
                   ($int-type)))
        
      ($zero?-exp (exp1)
                  (let ((type1 (typeof exp1 tenv)))
                    (check-equal-type! type1 ($int-type) exp1)
                    ($bool-type)))
        
      ($if-exp (exp1 exp2 exp3)
               (let ((ty1 (typeof exp1 tenv))
                     (ty2 (typeof exp2 tenv))
                     (ty3 (typeof exp3 tenv)))
                 (check-equal-type! ty1 ($bool-type) exp1)
                 (check-equal-type! ty2 ty3 exp)
                 ty2))

      ($let-exp (var exp1 body)
                (let ((ty1 (typeof exp1 tenv)))
                  (typeof body ($extend-tenv var ty1 tenv))))
        
      ($proc-exp (bvar bvar-type body)
                 (let ((expanded-bvar-type (expand-type bvar-type tenv)))
                   (let ((result-type (typeof body ($extend-tenv bvar expanded-bvar-type tenv))))
                     ($proc-type expanded-bvar-type result-type))))
        
      ($call-exp (rator rand) 
                 (let ((rator-type (typeof rator tenv))
                       (rand-type  (typeof rand tenv)))
                   (cases Type rator-type
                     ($proc-type (arg-type result-type)
                                 (begin
                                   (check-equal-type! arg-type rand-type rand)
                                   result-type))
                     (else (eopl:error 'type-of "Rator not a proc type:~%~s~%had rator type ~s" rator (type-to-external-form rator-type))))))
        
      ($letrec-exp (proc-result-type proc-name bvar bvar-type proc-body letrec-body)
                   (let ((tenv-for-letrec-body ($extend-tenv  proc-name (expand-type ($proc-type bvar-type proc-result-type) tenv) tenv)))
                     (let ((proc-result-type (expand-type proc-result-type tenv))
                           (proc-body-type (typeof proc-body ($extend-tenv bvar (expand-type bvar-type tenv) tenv-for-letrec-body))))
                       (check-equal-type! proc-body-type proc-result-type proc-body)
                       (typeof letrec-body tenv-for-letrec-body))))

      ; ---------------
      ($qualified-var-exp (m-name var-name) 
                          (lookup-qualified-var-in-tenv m-name var-name tenv))
        
      ))

  )
