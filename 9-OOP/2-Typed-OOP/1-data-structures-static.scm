(module static-data-structures (lib "eopl.ss" "eopl")

  ;; type environments and associated procedures.
  ;; In chapter7/checked, this is in checker.scm.

  (require "0-lang.scm")

  (provide (all-defined-out))

;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

  (define-datatype TEnv TEnv?
    ($empty-tenv)
    ($extend-tenv
      (syms (list-of symbol?))
      (vals (list-of Type?))
      (tenv TEnv?))
    ($extend-tenv-with-self-and-super
      (self Type?)
      (super-name symbol?)
      (saved-env TEnv?)))

  (define init-tenv
    (lambda ()
      ($extend-tenv
        '(i v x)
        (list ($int-type) ($int-type) ($int-type))
        ($empty-tenv))))

  (define apply-tenv
    (lambda (env search-sym)
      (cases TEnv env
        ($empty-tenv ()
          (eopl:error 'apply-tenv "No type found for ~s" search-sym))
        ($extend-tenv (bvars types saved-env)
          (cond
            ((location search-sym bvars)
             => (lambda (n) (list-ref types n)))
            (else
              (apply-tenv saved-env search-sym))))
        ($extend-tenv-with-self-and-super (self-name super-name saved-env)
          (case search-sym
            ((%self) self-name)
            ((%super) super-name)
            (else (apply-tenv saved-env search-sym)))))))

  (define location
    (lambda (sym syms)
      (cond
        ((null? syms) #f)
        ((eqv? sym (car syms)) 0)
        ((location sym (cdr syms)) => (lambda (n) (+ n 1)))
        (else #f))))

  ; ================================================================== 
  (define type->class-name
    (lambda (ty)
      (cases Type ty
        ($class-type (name) name)
        (else (eopl:error 'type->class-name
                "Not a class type: ~s"
                ty)))))

  (define class-type?
    (lambda (ty)
      (cases Type ty
        ($class-type (name) #t)
        (else #f))))

  (define type-to-external-form
    (lambda (ty)
      (cases Type ty
        ($int-type () 'int)
        ($bool-type () 'bool)
        ($void-type () 'void)
        ($class-type (name) name)
        ($list-type (ty) (list 'listof (type-to-external-form ty)))
        ($proc-type (arg-types result-type)
          (append
            (formal-types-to-external-form arg-types)
            '(->)
            (list (type-to-external-form result-type)))))))

  (define formal-types-to-external-form
    (lambda (types)
      (if (null? types)
        '()
        (if (null? (cdr types))
          (list (type-to-external-form (car types)))
          (cons
            (type-to-external-form (car types))
            (cons '*
              (formal-types-to-external-form (cdr types))))))))
)
