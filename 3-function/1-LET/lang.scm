(module lang (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  ;===========================================================
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '(
      (Program (Expression) $Program.a-program)
      ; --------------- Expression ---------------------
      (Expression (number) $Expr.const-exp)
      (Expression (identifier) $Expr.var-exp)
      (Expression ("-" "(" Expression "," Expression ")") $Expr.diff-exp)
      (Expression ("zero?" "(" Expression ")") $Expr.zero?-exp)
      (Expression ("if" Expression "then" Expression "else" Expression) $Expr.if-exp)
      (Expression ("let" identifier "=" Expression "in" Expression) $Expr.let-exp)
      ))

  ;=========================================================== SLLGEN
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  (define sp scan&parse)
  )
