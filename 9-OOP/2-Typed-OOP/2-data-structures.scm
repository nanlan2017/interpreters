(module data-structures (lib "eopl.ss" "eopl")

  (require "0-lang.scm")                  ; for expression?
  (require "0-store.scm")                 ; for reference?
  (require "2-classes.scm")               ; for object?

  (provide (all-defined-out))
  ;;;;;;;;;;;;;   ExpVal , Proc,  Env
  ; =================================================  expressed values
  (define-datatype ExpVal ExpVal?
    ($num-val
     (value number?))
    ($bool-val
     (boolean boolean?))
    ($proc-val 
     (proc Proc?))
    ($ref-val
     (ref reference?))
    ($obj-val
     (obj Object?))
    ($list-val
     (lst (list-of ExpVal?)))
    )

  ; extractors:

  (define expval->num
    (lambda (v)
      (cases ExpVal v
        ($num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases ExpVal v
        ($bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases ExpVal v
        ($proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))

  ;; not used.  Nor is expval->obj or expval->list, so we haven't written them.
  (define expval->ref
    (lambda (v)
      (cases ExpVal v
        ($ref-val (ref) ref)
        (else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

  ; =================================================== procedures

  (define-datatype Proc Proc?
    ($procedure
     (vars (list-of symbol?))
     (body Expression?)
     (env Env?)))
  
  (define-datatype Env Env?
    ($empty-env)
    ($extend-env 
     (bvars (list-of symbol?))
     (bvals (list-of reference?)) 
     (saved-env Env?))
    ($extend-env-rec**
     (proc-names (list-of symbol?))
     (b-varss (list-of (list-of symbol?)))
     (proc-bodies (list-of Expression?))
     (saved-env Env?))
    ($extend-env-with-self-and-super  ; binds :  %self ~~ an object  |  %super ~~ a class name
     (self Object?)
     (super-name symbol?)
     (saved-env Env?))
    )

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases Env env
        ($empty-env () '())
        ($extend-env (sym val saved-env)
                     (cons
                      (list sym val)
                      (env->list saved-env)))
        ($extend-env-rec** (p-names b-varss p-bodies saved-env)
                           (cons
                            (list 'letrec p-names '...)
                            (env->list saved-env)))
        ($extend-env-with-self-and-super (self super-name saved-env)
                                         (cons
                                          (list 'self self 'super super-name)
                                          (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned up with env->list 
  (define expval->printable
    (lambda (val)
      (cases ExpVal val
        ($proc-val (p)
                   (cases Proc p
                     ($procedure (var body saved-env)
                                 (list 'procedure var '... (env->list saved-env)))))
        (else val))))
		  (define init-env 
    (lambda ()
      (extend-env1
       'i (newref ($num-val 1))
       (extend-env1
        'v (newref ($num-val 5))
        (extend-env1
         'x (newref ($num-val 10))
         ($empty-env))))))

  (define extend-env1
    (lambda (id val env)
      ($extend-env (list id) (list val) env)))

  ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define apply-env
    (lambda (env search-sym)
      (cases Env env
        ($empty-env ()
                    (eopl:error 'apply-env "No binding for ~s" search-sym))
        ($extend-env (bvars bvals saved-env)
                     (cond
                       ((location search-sym bvars)
                        => (lambda (n)
                             (list-ref bvals n)))
                       (else
                         (apply-env saved-env search-sym))))
        ($extend-env-rec** (p-names b-varss p-bodies saved-env)
                           (cond 
                             ((location search-sym p-names)
                              => (lambda (n)
                                   (newref ($proc-val ($procedure  (list-ref b-varss n) (list-ref p-bodies n) env)))))
                             (else (apply-env saved-env search-sym))))
        ($extend-env-with-self-and-super (self super-name saved-env)
                                         (case search-sym
                                           ((%self) self)
                                           ((%super) super-name)
                                           (else (apply-env saved-env search-sym)))))))

  ;; location : Sym * Listof(Sym) -> Maybe(Int)
  ;; (location sym syms) returns the location of sym in syms or #f is sym is not in syms.
  (define location
    (lambda (sym syms)
      (cond
        ((null? syms) #f)
        ((eqv? sym (car syms)) 0)
        ((location sym (cdr syms)) => (lambda (n) (+ n 1)))
        (else #f))))


  )
