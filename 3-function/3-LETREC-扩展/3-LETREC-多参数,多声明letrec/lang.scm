(module lang (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)      
      (comment ("%" (arbno (not #\newline)))skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '(
      (Program (Expression) $a-program)
      ; ------------- Expression ------------------------
      (Expression (number) $const-exp)
      (Expression (identifier) $var-exp)
      (Expression ("-" "(" Expression "," Expression ")")  $diff-exp)
      (Expression ("zero?" "(" Expression ")") $zero?-exp)
      (Expression ("if" Expression "then" Expression "else" Expression)  $if-exp)
      (Expression ("let" identifier "=" Expression "in" Expression) $let-exp)
      
      (Expression ("proc" "(" (arbno identifier) ")" Expression) $proc-exp)
      (Expression ("(" Expression (arbno Expression) ")") $call-exp)
      (Expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" Expression) "in" Expression) $letrec-exp)
      ))
      
  ;================================================================== SLLGEN
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  (define sp scan&parse)
  
  )
