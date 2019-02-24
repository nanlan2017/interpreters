(module lang (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)      
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '((program (expression) a-program)
      ;------------------------- Expression -------------------------------
      (expression (number) const-exp)
      (expression (identifier) var-exp)
      (expression ("proc" "(" (arbno identifier)")" expression) proc-exp)
      (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
      (expression ("list" "(" (separated-list number ",") ")") const-list-exp)
      
      (expression ("-" "(" expression "," expression ")")  diff-exp)      
      (expression ("if" expression "then" expression "else" expression)  if-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)            
      (expression ("(" expression (arbno expression) ")") call-exp)
      (expression (unary-op "(" expression ")") unary-op-exp)

      (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
      (expression ("print" number) print-num-exp)

      ; Exception Handling
      (expression ("try" expression "catch" "(" identifier ")" expression) try-exp)
      (expression ("raise" expression) raise-exp)
      ; -------------------------
      (unary-op ("zero?") zero?-op)
      (unary-op ("null?") null?-op)
      (unary-op ("car") car-op)
      (unary-op ("cdr") cdr-op)
      ))
      
  ;;================================================================== SLLGEN
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar)) 
  (define sp scan&parse)
  )
