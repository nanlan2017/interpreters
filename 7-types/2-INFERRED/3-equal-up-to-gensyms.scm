(module equal-up-to-gensyms (lib "eopl.ss" "eopl")

  (provide equal-types?)

  ; (equal-types? '(t1 -> t1) '(t2 -> t2))  === #t
  ; 相当于把所有的 type expression 中所有的   不同的type var : t1， t3 都依次替换为 a , b ,c , d ..

  ; =========================================================================
  (define (equal-types? ty1 ty2)
    (equal-up-to-gensyms? ty1 ty2))

  ;; S-exp = Sym | Listof(S-exp)
  ;; A-list = Listof(Pair(TvarTypeSym, TvarTypesym))
  ;; a tvar-type-sym is a symbol ending with a digit.

  ;; equal-up-to-gensyms? : S-exp * S-exp -> Bool
  (define (equal-up-to-gensyms? sexp1 sexp2)
    (equal? (rewrite sexp1)
            (rewrite sexp2)))

  ; ███> (rewrite '(int -> bool -> t1 -> t5 -> t1))
  ; '(int -> bool -> a -> b -> a)
  (define (rewrite sexp)
    (apply-subst-to-sexp (canonical-subst sexp) sexp))

  ;; canonicalize : S-exp -> A-list
  ;; usage: replaces all tvar-syms with tvar1, tvar2, etc.
  (define (canonical-subst sexp)
    ;; loop : sexp * alist -> alist
    (let loop ((sexp sexp)
               (table '()))
      (cond
        ((null? sexp) table)
        ((tvar-type-sym? sexp) (cond 
                                 ((assq sexp table) table)   ; sexp is already bound, no more to do
                                 (else (cons (cons sexp (n->ty (length table)))  ;; the length of the table serves as a counter!
                                             table))))
        ((pair? sexp) (loop (cdr sexp)
                            (loop (car sexp) table)))
        (else table))))


  ;; apply-subst-to-sexp : A-list * S-exp -> S-exp
  (define (apply-subst-to-sexp subst sexp)
    (cond
      ((null? sexp) sexp)
      ((tvar-type-sym? sexp) (cdr (assq sexp subst)))
      ((pair? sexp) (cons (apply-subst-to-sexp subst (car sexp))
                          (apply-subst-to-sexp subst (cdr sexp))))
      (else sexp)))
  ;-------------------------------------------------------------------------------------------
  ; tvar-sym? t0 = true
  ; tvar-sym? ta = false
  ;; tvar-type-sym? : Sym -> Bool
  (define (tvar-type-sym? sym)
    (and (symbol? sym)
         (char-numeric? (car (reverse (symbol->list sym))))))

  ;; symbol->list : Sym -> List
  (define symbol->list
    (lambda (x) (string->list (symbol->string x))))  

  
  ;; ctr->ty 3 = 'tvar3
  ;; ctr->ty : N -> Sym
  ;  (define (ctr->ty n)
  ;    (string->symbol (string-append "tvar" (number->string n))))
  (define alpha '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  
  (define (n->ty n)
    (list-ref alpha n))


  )