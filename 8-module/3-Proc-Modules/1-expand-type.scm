(module expand-type (lib "eopl.ss" "eopl")

  (require "0-lang.scm")
  (require "1-data-structures-static.scm")

  (provide expand-type
    expand-iface)
  ;=============================================================================
  ; expand-type :: Type * TEnv -> ExpandedType
  (define (expand-type ty tenv)
    (cases Type ty
      ($int-type ()
                 ($int-type))
      ($bool-type ()
                  ($bool-type))
      ($proc-type (arg-type result-type)
                 ($proc-type (expand-type arg-type tenv) (expand-type result-type tenv)))
      ; 核心：化名type / 抽象type 
      ($named-type (name)
                  (lookup-type-name-in-tenv tenv name))
      ($qualified-type (m-name t-name)
                      (lookup-qualified-type-in-tenv m-name t-name tenv))
      ))

  (define (lookup-type-name-in-tenv tenv name)
    (cases TEnv tenv
      ($extend-tenv-with-type (ty etype saved-tenv)
                              (cases Type ty
                                ($named-type (id)
                                             (if (eqv? name id)
                                                 etype
                                                 (lookup-type-name-in-tenv saved-tenv name)))
                                (else (lookup-type-name-in-tenv saved-tenv name))))
      (else (lookup-type-name-in-tenv (get-nested-tenv tenv) name))))
  
  (define (lookup-qualified-type-in-tenv m-name t-name tenv)
    (cases TEnv tenv
      ($extend-tenv-with-type (ty etype saved-tenv)
                              (cases Type ty
                                ($qualified-type (mod-id ty-id)
                                             (if (and (eqv? m-name mod-id) (eqv? t-name ty-id))
                                                 etype
                                                 (lookup-qualified-type-in-tenv m-name t-name saved-tenv)))
                                (else (lookup-qualified-type-in-tenv m-name t-name saved-tenv))))
      (else (lookup-qualified-type-in-tenv m-name t-name (get-nested-tenv tenv)))))
  ;-----------------------------------------------------------------------------
    
  
  (define (expand-iface m-name iface tenv)
    iface)

  )
