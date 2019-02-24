(module transform-CPS-IN-to-CPS-OUT (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang-in-out.scm")
  (require "utils.scm")
  ; ****************************************************************************************************************
  ;  CPS-IN src  ----->  AST of CPS-IN
  ;                   ---------【Transformer】------->  AST of CPS-OUT
  ;                                                                   ------> eval AST of CPS-OUT
  ;
  ;
  ;       "lang-CPS-IN.scm"       "cps.scm"            "lang-CPS-OUT.scm"      "data-structures.scm" & "interp.scm"
  ;
  ; ****************************************************************************************************************
  
  ;; inp-exp-simple? : InpExp -> Bool
  ;; returns #t or #f, depending on whether exp would be a simple-exp if reparsed using the CPS-OUT language.
  (define inp-exp-simple?
    (lambda (exp)
      (cases expression exp
        (const-exp (num) #t)
        (var-exp (var) #t)
        (diff-exp (exp1 exp2)
                  (and
                   (inp-exp-simple? exp1)
                   (inp-exp-simple? exp2)))
        (zero?-exp (exp1)
                   (inp-exp-simple? exp1))
        (proc-exp (ids exp) #t)
        (sum-exp (exps)
                 (all-simple? exps))
        (else #f))))

  (define all-simple?
    (lambda (exps)
      (if (null? exps)
          #t
          (and (inp-exp-simple? (car exps))
               (all-simple? (cdr exps))))))

  ;; takes a list of expressions and finds the position of the first one that is not a simple-exp, else returns #f
  ;  (define index-of-first-non-simple
  ;    (lambda (exps)
  ;      (cond
  ;        ((null? exps) #f)
  ;        ((inp-exp-simple? (car exps)) (let ((pos (index-of-first-non-simple (cdr exps))))
  ;                                        (if pos
  ;                                            (+ pos 1)
  ;                                            #f)))
  ;        (else 0))))  
  ; ================================================================================================================
  
  (define (cpsfy src)
    (cps-of-program (scan&parse src)))
  
  ;; cps-of-program : InpExp -> TfExp
  ;; Page: 224
  (define cps-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (a-cps-program (cps-of-exps (list exp1) (lambda (new-args)
                                                             ($simple-exp->exp (car new-args)))))))))

  ;; cps-of-exps : Listof(InpExp) * (Listof(InpExp) -> TfExp)  -> TfExp
  ;; Page: 219
  
  ;; usage: 
  ;;   -- assume e_i's are non-simple, b_i's are simple
  ;;   -- then 
  ;;        (cps-of-exps '(b1 b2 e1 b3 e2 e3) F) == [e1](\v1.[e2](\v2.[e3](\v3.(F `(,<b1> ,<b2> ,v1 ,<b3> ,v2 ,v3)))))
  ;;                                  where <b> is cps-of-simple-exp of b.
  (define cps-of-exps
    (lambda (exps builder)
      (let cps-of-rest ((exps exps)) ;; cps-of-rest : Listof(InpExp) -> TfExp        
        (let ((pos (list-index (lambda (exp) (not (inp-exp-simple? exp))) exps)))
          (if (not pos)
              (builder (map cps-of-simple-exp exps))
              (let ((var (fresh-identifier 'v)))
                (cps-of-exp
                 (list-ref exps pos)
                 (@proc-exp (list var)
                            (cps-of-rest
                             (list-set exps pos (var-exp var)))))))))))

  ;================================================================================================================
  ;; cps-of-simple-exp : InpExp -> SimpleExp
  ;; Page: 220
  ;; assumes (inp-exp-simple? exp).
  (define cps-of-simple-exp
    (lambda (exp)
      (cases expression exp
        (const-exp (num)
                   (@const-exp num))
        (var-exp (var)
                 (@var-exp var))
        (diff-exp (exp1 exp2)
                  (@diff-exp (cps-of-simple-exp exp1)
                             (cps-of-simple-exp exp2)))
        (zero?-exp (exp1)
                   (@zero?-exp (cps-of-simple-exp exp1)))
        (proc-exp (ids exp) 
                  (@proc-exp (append ids (list 'k%00))
                             (cps-of-exp exp (@var-exp 'k%00))))
        (sum-exp (exps)
                 (@sum-exp (map cps-of-simple-exp exps)))
        
        (else (report-invalid-exp-to-cps-of-simple-exp exp)))))

  (define report-invalid-exp-to-cps-of-simple-exp
    (lambda (exp)
      (eopl:error 'cps-simple-of-exp "non-simple expression to cps-of-simple-exp: ~s" exp)))

  ;; cps-of-exp : Exp * SimpleExp -> TfExp
  ;; Page: 222
  (define cps-of-exp
    (lambda (exp cont)
      (cases expression exp
        ; --------------------------------- make-send-to-cont : const, var, proc
        (const-exp (num)
                   (make-send-to-cont cont
                                      (@const-exp num)))
        (var-exp (var)
                 (make-send-to-cont cont
                                    (@var-exp var)))
        (proc-exp (vars body) 
                  (make-send-to-cont cont
                                     (@proc-exp (append vars (list 'k%00)) (cps-of-exp body (@var-exp 'k%00)))))
        ; --------------------------------- cps-of-XXX exp : zero? , diff, sum , if, let, letrec, call
        (zero?-exp (exp1)
                   (cps-of-zero?-exp exp1 cont))
        (diff-exp (exp1 exp2)
                  (cps-of-diff-exp exp1 exp2 cont))
        (sum-exp (exps)
                 (cps-of-sum-exp exps cont))
        (if-exp (exp1 exp2 exp3)
                (cps-of-if-exp exp1 exp2 exp3 cont))
        (let-exp (var exp1 body)
                 (cps-of-let-exp var exp1 body cont))
        (letrec-exp (ids bidss proc-bodies body)
                    (cps-of-letrec-exp ids bidss proc-bodies body cont))
        (call-exp (rator rands)
                  (cps-of-call-exp rator rands cont)))))

  ;; make-send-to-cont : SimpleExp * SimpleExp -> TfExp
  ;; Page: 214
  (define make-send-to-cont
    (lambda (cont bexp)
      ($call-exp cont (list bexp))))

  ;; cps-of-zero?-exp : InpExp * SimpleExp -> TfExp
  ;; Page: 222
  (define cps-of-zero?-exp
    (lambda (exp1 k-exp)
      (cps-of-exps (list exp1)
                   (lambda (new-rands)
                     (make-send-to-cont
                      k-exp
                      (@zero?-exp 
                       (car new-rands)))))))

  ;; cps-of-sum-exp : Listof (InpExp) * SimpleExp -> TfExp
  ;; Page: 219
  (define cps-of-sum-exp
    (lambda (exps k-exp)
      (cps-of-exps exps
                   (lambda (new-rands)
                     (make-send-to-cont
                      k-exp
                      (@sum-exp new-rands))))))

  ;; cps-of-diff-exp : InpExp * InpExp * SimpleExp -> TfExp
  ;; Page: 223
  (define cps-of-diff-exp
    (lambda (exp1 exp2 k-exp)
      (cps-of-exps
       (list exp1 exp2)
       (lambda (new-rands)
         (make-send-to-cont
          k-exp
          (@diff-exp
           (car new-rands)
           (cadr new-rands)))))))

  ;; cps-of-if-exp : InpExp * InpExp * InpExp * SimpleExp -> TfExp
  ;; Page: 223
  (define cps-of-if-exp
    (lambda (exp1 exp2 exp3 k-exp)
      (cps-of-exps (list exp1)
                   (lambda (new-rands)
                     ($if-exp (car new-rands)
                              (cps-of-exp exp2 k-exp)
                              (cps-of-exp exp3 k-exp))))))

  ;; cps-of-let-exp : Var * InpExp * InpExp * SimpleExp -> TfExp
  ;; Page: 222
  (define cps-of-let-exp
    (lambda (id rhs body k-exp)
      (cps-of-exps (list rhs)
                   (lambda (new-rands)
                     ($let-exp id 
                               (car new-rands)
                               (cps-of-exp body k-exp))))))

  ;; cps-of-letrec-exp :  Listof(Listof(Var)) * Listof(InpExp) * InpExp * SimpleExp -> TfExp
  ;; Page: 223
  (define cps-of-letrec-exp
    (lambda (proc-names idss proc-bodies body k-exp)
      ($letrec-exp
       proc-names
       (map (lambda (ids) (append ids (list 'k%00))) idss)
       (map (lambda (exp) (cps-of-exp exp (@var-exp 'k%00))) proc-bodies)
       (cps-of-exp body k-exp))))

  ;; cps-of-call-exp : InpExp * Listof(InpExp) * SimpleExp -> TfExp
  ;; Page: 220
  (define cps-of-call-exp
    (lambda (rator rands k-exp)
      (cps-of-exps (cons rator rands)
                   (lambda (new-rands)
                     ($call-exp
                      (car new-rands)
                      (append (cdr new-rands) (list k-exp)))))))
  )
