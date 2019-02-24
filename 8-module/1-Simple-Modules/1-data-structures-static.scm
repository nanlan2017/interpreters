(module data-structures-static (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "0-lang.scm")

  ; ========================================================================= tenv (symbol <-> type)
  
  (define-datatype TEnv TEnv?
    ($empty-tenv)
    ($extend-tenv
     (var symbol?)
     (ty Type?)
     (tenv TEnv?))
    ; module
    ($extend-tenv-with-module
     (name symbol?)
     (face SimpleInterface?)
     (tenv TEnv?))
    )

  (define (get-nested-tenv tenv)
    (cases TEnv tenv
      ($empty-tenv ()
                   (eopl:error 'get-nested-tenv "No nested tenv for Empty-tenv !"))
      ($extend-tenv (saved-var saved-ty saved-tenv)
                    saved-tenv)
      ($extend-tenv-with-module (mod-name face saved-tenv)
                                saved-tenv)
      ))

  (define (init-tenv)
    ($empty-tenv))
  
  (define (apply-tenv tenv var)
    (cases TEnv tenv
      ($empty-tenv ()
                   (eopl:error 'apply-tenv "Didn't find in type-env while search : ~s" var))
      ($extend-tenv (saved-var saved-ty saved-tenv)
                    (if (equal? var saved-var)
                        saved-ty
                        (apply-tenv saved-tenv var)))
      ($extend-tenv-with-module (mod-name face saved-tenv)
                                (apply-tenv saved-tenv var))

      ))
  ; ------------------------------------------------------------------------- 查找TEnv
  ; 1. 查找 m1::a 的type 
  ; lookup-qualified-var-in-tenv :: [m::a] * TEnv -> Type                      
  (define (lookup-qualified-var-in-tenv m-name var-name tenv)
    (let ((iface (lookup-tenv/id=>iface tenv m-name)))
      (cases SimpleInterface iface
        ($a-simple-interface (decls)
                             (lookup-decls/var=>type var-name decls)))))
  
  ; 2. module-id  ==> Interface
  ; lookup-module-name-in-tenv :: TEnv * Symbol -> Interface
  (define (lookup-tenv/id=>iface tenv m-name)
    (cases TEnv tenv
      ($extend-tenv-with-module (mod-id face saved-tenv)
                                (if (eqv? m-name mod-id)
                                    face
                                    (lookup-tenv/id=>iface saved-tenv m-name)))
      (else (lookup-tenv/id=>iface (get-nested-tenv tenv) m-name))))

  ; 3. Interface(decls) ==> type
  ; lookup-variable-name-in-decls :: Symbol * [VarDeclartion] -> Type
  (define (lookup-decls/var=>type var-name decls)
    (if (null? decls)
        (eopl:error 'lookup-variable-name-in-decls "Didn't find ~s in decls~n" var-name)
        (cases VarDeclaration (car decls)
          ($a-var-declaration (var ty)
                              (if (eqv? var var-name)
                                  ty
                                  (lookup-decls/var=>type var-name (cdr decls)))))))
  ; =========================================================================
  ;; type-to-external-form : Type -> List
  (define type-to-external-form
    (lambda (ty)
      (cases Type ty
        ($int-type ()
                   'int)
        ($bool-type ()
                    'bool)
        ($proc-type (arg-type result-type)
                    (list (type-to-external-form arg-type) '-> (type-to-external-form result-type)))
        )))
  ; =========================================================================
  (define (decl->name decl)
    (cases VarDeclaration decl
      ($a-var-declaration (var ty)
                          var)))
  
  (define (decl->type decl)
    (cases VarDeclaration decl
      ($a-var-declaration (var ty)
                          ty)))
  ; =========================================================================
  ;; check-equal-type! : Type * Type * Exp -> Unspecified
  (define check-equal-type!
    (lambda (ty1 ty2 exp)
      (when (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp))))

  ;; report-unequal-types : Type * Type * Exp -> Unspecified
  (define report-unequal-types
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-equal-type!  "Types didn't match: ~s != ~a in~%~a"  (type-to-external-form ty1) (type-to-external-form ty2) exp)))  
  )
