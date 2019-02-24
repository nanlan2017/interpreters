(module pprinter-CPS-OUT (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang-in-out.scm")
  (require "transform-CPS-IN-to-CPS-OUT.scm")

  ; (strlist-concat-with '("a" "b" "c") ",")
  (define (concat-with slist separator)
    (let loop ([lst (cdr slist)]
               [acc (car slist)])
      (if (null? lst)
          acc
          (loop (cdr lst) (string-append acc separator (car lst))))))
  ; ========================================================================================
  (define (rewrite src)
    (display src)
    (eopl:printf "~n~n")
    (newline)
    (display (cps-program->string (cps-of-program (scan&parse src)))))

  ; (cps-out-program (tfexp) cps-a-program)
  (define (cps-program->string pgm)
    (cases CpsProgram pgm
      (a-cps-program (exp1)
                     (tfexp->string exp1))))

  (define (sexp->string sexp)
    (cases SimpleExp sexp
      ; (simple-expression (number) cps-const-exp)
      ; (simple-expression (identifier) cps-var-exp)
      (@const-exp (num)
                  (number->string num)
                  )        
      (@var-exp (var)
                (symbol->string var)
                )
      ; (simple-expression ("-" "(" simple-expression "," simple-expression ")") cps-diff-exp)
      ; (simple-expression ("+" "(" (separated-list simple-expression ",") ")") cps-sum-exp)
      (@diff-exp (exp1 exp2)
                 (string-append "-(" (sexp->string exp1) ", " (sexp->string exp2) ")")
                 )
      (@sum-exp (exps)
                (let [(strlist (map sexp->string exps))]
                  (string-append "+(" (concat-with strlist ", ") ")")
                  ))
      ; (simple-expression ("zero?" "(" simple-expression ")") cps-zero?-exp)      
      ; (simple-expression ("proc" "(" (arbno identifier) ")" tfexp) cps-proc-exp)
      (@zero?-exp (exp1)
                  (string-append "zero?(" (sexp->string exp1) ")")
                  )      
      (@proc-exp (vars body)
                 (let* [(varlist (map symbol->string vars))
                        (params-str (string-append "(" (concat-with varlist ",") ")"))]
                   (string-append "proc" params-str " => " "\n" (tfexp->string body))
                   ))
      ))

  (define (tfexp->string texp)
    (cases TailExp texp
      ; (tfexp (simple-expression) simple-exp->exp)
      ($simple-exp->exp (simple)
                        (sexp->string simple)
                        )
      ; (tfexp ("let" identifier "=" simple-expression "in" tfexp) cps-let-exp)   
      ($let-exp (var rhs body)
                (string-append "let" (symbol->string var) " = " (sexp->string rhs) " in " (tfexp->string body))
                )
      ; (tfexp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tfexp) "in" tfexp) cps-letrec-exp)
      ($letrec-exp (p-names b-varss p-bodies letrec-body)
                   (let [(functions (let loop ([pids p-names]
                                               [vars-s b-varss]
                                               [bodys p-bodies]
                                               [acc ""])
                                      (if (null? pids)
                                          acc
                                          (loop (cdr pids) (cdr vars-s) (cdr bodys)
                                                (let [(id (car pids))
                                                      (vars (car vars-s))
                                                      (body (car bodys))]
                                                  (string-append acc
                                                                 (symbol->string id)
                                                                 " (" (concat-with (map symbol->string vars) " ") ") "
                                                                 "="
                                                                 (tfexp->string body)
                                                                 ))))))]
                        
                     (string-append "letrec " functions " in " (tfexp->string letrec-body))
                     ))
      ; (tfexp ("if" simple-expression "then" tfexp "else" tfexp) cps-if-exp)
      ($if-exp (simple1 body1 body2)
               (string-append "if " (sexp->string simple1) " then " (tfexp->string body1) " else " (tfexp->string body2))
               )
      ; (tfexp ("(" simple-expression (arbno simple-expression) ")")  cps-call-exp)
      ($call-exp (rator rands)
                 (string-append "(" (sexp->string rator) " " (concat-with (map sexp->string rands) " ") ")" )
                 )
      ))
  )
