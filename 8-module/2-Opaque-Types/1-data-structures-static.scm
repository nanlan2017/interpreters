(module lang-type (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "0-lang.scm")
  ;;============================================================== TEnv
  ;  we introduce a systematic way of handling opaque and transparent types.
  ;  An opaque type behaves like a primitive type, such as int or bool.
  ;  Transparent types, on the other hand, are transparent, as the name suggests: they behave exactly like their definitions.
  ;  ████ So every type is equivalent to one that is given by the grammar
  ;             Type ::= int | bool | (Type -> Type) | from m take t   【type alias不是Type的“运行时”值,而像一个Type Ref而已】
  ;  where t is declared as an opaque type in m. We call a type of this form an expanded type.
  ;  We next extend type environments to handle new types. Our type environments will bind each named type or qualified type to an expanded type.
  
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
    ($extend-tenv-with-type  ; 对应body里的 $type-definition
     (name Type?)   ; 'literal'
     (type Type?)   ; ★ expanded-type
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
      ($extend-tenv-with-type (ty etype saved-tenv)
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
      ($extend-tenv-with-type (ty etype saved-tenv)
                              (apply-tenv saved-tenv var))

      ))
  ;----------------------------------------------------------- Observer: 在TEnv中查找module内的var的type
  ; lookup-module-name-in-tenv :: TEnv * Symbol -> ModuleInterface
  (define (lookup-tenv/id->iface tenv m-name)
    (cases TEnv tenv
      ($extend-tenv-with-module (mod-id face saved-tenv)
                                (if (eqv? m-name mod-id)
                                    face
                                    (lookup-tenv/id->iface saved-tenv m-name)))
      (else (lookup-tenv/id->iface (get-nested-tenv tenv) m-name))))

  ; lookup-qualified-var-in-tenv :: Symbol * Symbol * TEnv -> Type                      
  (define (lookup-tenv/qualified-var=>type m-name var-name tenv)
    (let ((iface (lookup-tenv/id->iface tenv m-name)))
      (cases SimpleInterface iface
        ($a-simple-interface (decls)
                             (lookup-decls/var=>type var-name decls)))))

  ; lookup-variable-name-in-decls :: Symbol * [VarDeclaration] -> Type
  (define (lookup-decls/var=>type var-name decls)
    (cases VarDeclaration (car decls)
      ($a-var-declaration (var ty)
                          (if (eqv? var var-name)
                              ty
                              (lookup-decls/var=>type var-name (cdr decls))))
      (else (lookup-decls/var=>type var-name (cdr decls)))))  
  ; =============================================================================
  ;; check-equal-type! : Type * Type * Exp -> Unspecified
  (define check-equal-type!
    (lambda (ty1 ty2 exp)
      (when (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp))))

  ;; report-unequal-types : Type * Type * Exp -> Unspecified
  (define report-unequal-types
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-equal-type!  "Types didn't match: ~s != ~a in~%~a"  (type-to-external-form ty1) (type-to-external-form ty2) exp)))
  
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
        ;
        ($named-type (t)
                     t)
        ($qualified-type (mod-name ty)
                         (string->symbol (string-append (symbol->string mod-name) "::"(symbol->string ty))))
        )))
  ; =========================================================================== predicts, extractors
  (define atomic-type?
    (lambda (ty)
      (cases Type ty
        ($proc-type (ty1 ty2) #f)
        (else #t))))

  ; ------------ proc-type
  (define proc-type?
    (lambda (ty)
      (cases Type ty
        ($proc-type (t1 t2) #t)
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
        (else (eopl:error 'proc-type->result-types "Not a proc type: ~s" ty)))))
  
  ; ----------- module definition
  (define module-definition->name
    (lambda (m-defn)
      (cases ModuleDefinition m-defn
        ($a-module-definition (m-name m-type m-body)
                              m-name))))

  (define module-definition->interface
    (lambda (m-defn)
      (cases ModuleDefinition m-defn
        ($a-module-definition (m-name m-type m-body)
                              m-type))))

  (define module-definition->body
    (lambda (m-defn)
      (cases ModuleDefinition m-defn
        ($a-module-definition (m-name m-type m-body)
                              m-body))))
  ; ------------- var declaration
  (define val-decl?
    (lambda (decl)
      (cases VarDeclaration decl
        ($a-var-declaration (name ty) #t)
        (else #f))))

  (define transparent-type-decl?
    (lambda (decl)
      (cases VarDeclaration decl
        ($transparent-type-declaration (name ty) #t)
        (else #f))))

  (define opaque-type-decl?
    (lambda (decl)
      (cases VarDeclaration decl
        ($opaque-type-declaration (name) #t)
        (else #f))))

  (define decl->name
    (lambda (decl)
      (cases VarDeclaration decl
        ($opaque-type-declaration (name) name)
        ($transparent-type-declaration (name ty) name)
        ($a-var-declaration (name ty) name))))

  (define decl->type
    (lambda (decl)
      (cases VarDeclaration decl
        ($transparent-type-declaration (name ty) ty)
        ($a-var-declaration (name ty) ty)
        ($opaque-type-declaration (name) (eopl:error 'decl->type "can't take type of abstract type declaration ~s" decl)))))  
  ; ================================================================================  module definitions
  ;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Maybe(Defn)
  (define maybe-lookup-module-in-list
    (lambda (name module-defs)
      (if (null? module-defs)
          #f
          (let ((name1 (module-definition->name (car module-defs))))
            (if (eqv? name1 name)
                (car module-defs)
                (maybe-lookup-module-in-list name (cdr module-defs)))))))

  ;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Defn OR Error
  (define lookup-module-in-list
    (lambda (name module-defs)
      (cond
        ((maybe-lookup-module-in-list name module-defs) => (lambda (mdef) mdef))
        (else (eopl:error 'lookup-module-in-list "unknown module ~s" name)))))


       
  )
