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
    '((program (expression) a-program)
      ;------------------------- Expression -------------------------------
      (expression (number) const-exp)
      (expression (identifier) var-exp)
      (expression ("-" "(" expression "," expression ")")  diff-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression ("if" expression "then" expression "else" expression)  if-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)      
      (expression ("proc" "(" identifier")" expression) proc-exp)
      (expression ("(" expression expression ")") call-exp)      
      (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)

      (expression ("let2" identifier "=" expression "," identifier "=" expression "in" expression) let2-exp)
      (expression ("list" "(" (separated-list expression ",") ")") list-exp)
      ))
      
  ;;================================================================== SLLGEN
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  (define sp scan&parse)
  
  )
