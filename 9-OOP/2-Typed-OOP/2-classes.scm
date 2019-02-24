(module classes (lib "eopl.ss" "eopl")
  (require "0-lang.scm")
  (require "0-store.scm")
  

  ;; object interface
  (provide Object
           Object?
           new-object
           object->class-name
           object->fields
           )

  ;; method interface
  (provide Method
           Method?
           $a-method
           find-method
           )
  
  ;; class interface
  (provide lookup-class
           initialize-class-env!
           is-subclass?
           )
  ; ================================================================= objects
  ;; an object consists of a symbol denoting its class, 
  ;; and a list of references representing the managed storage for the all the fields. 
  
  (define identifier? symbol?)

  (define-datatype Object Object? 
    ($an-object
     (class-name identifier?)
     (fields (list-of reference?))))

  ;; new-object : ClassName -> Obj
  ;; Page 340
  (define new-object                      
    (lambda (class-name)
      ($an-object class-name (map 
                              (lambda (field-name) (newref (list 'uninitialized-field field-name)))
                              (class->field-names (lookup-class class-name))))))

  ; ================================================================= methods and method environments
  ; 对于一个method，为了能求值 obj1.method1(args)，相当于 eval body (extend-with args..) [self,super,field-s]
  (define-datatype Method Method?
    ($a-method
     (vars (list-of symbol?))
     (body Expression?)
     (super-name symbol?)   ; 需记录method中出现super时指的是哪个Class
     (field-names (list-of symbol?))))

  ;----------------------------------------------------------------- method environments
  ;; a method environment looks like ((method-name method) ...)
  (define MethodEnv?
    (list-of 
     (lambda (p)
       (and 
        (pair? p)
        (symbol? (car p))
        (Method? (cadr p))))))

  ;; method-env * id -> (maybe method)
  (define assq-method-env
    (lambda (m-env id)
      (cond
        ((assq id m-env) => cadr)
        (else #f))))

  ;; find-method : Sym * Sym -> Method
  ;; Page: 345
  (define find-method
    (lambda (c-name name)
      (let ((m-env (class->method-env (lookup-class c-name))))
        (let ((maybe-pair (assq name m-env)))
          (if (pair? maybe-pair) (cadr maybe-pair)
              (report-method-not-found name))))))

  (define report-method-not-found
    (lambda (name)
      (eopl:error 'find-method "unknown method ~s" name)))
  
  ;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
  ;; Page: 345
  (define merge-method-envs
    (lambda (super-m-env new-m-env)
      (append new-m-env super-m-env)))

  ;; method-decls->method-env :
  ;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
  ;; Page: 345
  (define method-decls->method-env
    (lambda (m-decls super-name field-names)
      (map
       (lambda (m-decl)
         (cases MethodDecl m-decl
           ($a-method-decl (res-type method-name vars vars-tys body)
                           (list method-name ($a-method vars body super-name field-names)))))
       m-decls)))

  ; ================================================================= classes
  ; 父类、fields、methods
  (define-datatype Class Class?
    ($a-class
     (super-name (maybe symbol?))
     (field-names (list-of symbol?))
     (method-env MethodEnv?)))

  ;--------------------------------------------------------------- class environments

  ;; the-class-env will look like ((class-name class) ...)

  ;; the-class-env : ClassEnv
  ;; Page: 343
  (define %%the-class-env '())

  ;; add-to-class-env! : ClassName * Class -> Unspecified
  ;; Page: 343
  (define add-to-class-env!
    (lambda (class-name class)
      (set! %%the-class-env
            (cons
             (list class-name class)
             %%the-class-env))))

  ;; lookup-class : ClassName -> Class
  (define lookup-class                    
    (lambda (name)
      (let ((maybe-pair (assq name %%the-class-env)))
        (if maybe-pair (cadr maybe-pair)
            (report-unknown-class name)))))

  (define report-unknown-class
    (lambda (name)
      (eopl:error 'lookup-class "Unknown class ~s" name)))

  (define (is-subclass? c1 c2)
    (cond
      [(eqv? c1 c2) #t]
      [else (let [(father1 (class->super-name (lookup-class c1)))]
              (if father1
                  (is-subclass? father1 c2)
                  #f))]))
 
  ;; constructing classes

  ;; initialize-class-env! : Listof(ClassDecl) -> Unspecified
  ;; Page: 344
  (define initialize-class-env!
    (lambda (c-decls)
      ; Object 根类
      (set! %%the-class-env (list (list 'object ($a-class #f '() '()))))
      (for-each initialize-class-decl! c-decls)))

  ;; initialize-class-decl! : ClassDecl -> Unspecified
  (define initialize-class-decl!
    (lambda (c-decl)
      (cases ClassDecl c-decl
        ($a-class-decl (c-name father-name iface-names f-types f-names m-decls)
                       (let ((f-names (append-field-names (class->field-names (lookup-class father-name)) f-names)))
                         (add-to-class-env! c-name ($a-class father-name
                                                             f-names
                                                             (merge-method-envs
                                                              (class->method-env (lookup-class father-name))
                                                              (method-decls->method-env m-decls father-name f-names))))))
        ($an-interface-decl (iface-name abs-m-decls)
                            #f)
        )))  

  ;; exercise:  rewrite this so there's only one set! to the-class-env.

  ;; append-field-names :  Listof(FieldName) * Listof(FieldName) 
  ;;                       -> Listof(FieldName)
  ;; Page: 344
  ;; like append, except that any super-field that is shadowed by a
  ;; new-field is replaced by a gensym
  (define append-field-names
    (lambda (super-fields new-fields)
      (cond
        ((null? super-fields) new-fields)
        (else
          (cons 
           (if (memq (car super-fields) new-fields)
               (fresh-identifier (car super-fields))
               (car super-fields))
           (append-field-names
            (cdr super-fields) new-fields))))))

  ; ================================================================= selectors

  (define class->super-name
    (lambda (c-struct)
      (cases Class c-struct
        ($a-class (super-name field-names method-env)
                  super-name))))

  (define class->field-names
    (lambda (c-struct)
      (cases Class c-struct
        ($a-class (super-name field-names method-env)
                  field-names))))

  (define class->method-env
    (lambda (c-struct)
      (cases Class c-struct
        ($a-class (super-name  field-names method-env)
                  method-env))))

  ; ---------------------------------------------------------------
  (define object->class-name
    (lambda (obj)
      (cases Object obj
        ($an-object (class-name fields)
                    class-name))))

  (define object->fields
    (lambda (obj)
      (cases Object obj
        ($an-object (class-decl fields)
                    fields))))

  ; a%1 , b%2 ...
  (define fresh-identifier
    (let ((sn 0))
      (lambda (identifier)  
        (set! sn (+ sn 1))
        (string->symbol
         (string-append
          (symbol->string identifier)
          "%"             ; this can't appear in an input identifier
          (number->string sn))))))

  ; Maybe T == #f | T
  (define maybe
    (lambda (pred)
      (lambda (v)
        (or (not v) (pred v)))))

  )