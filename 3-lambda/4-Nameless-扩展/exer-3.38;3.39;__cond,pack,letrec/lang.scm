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
      (program (expression) a-program)

      (expression (number) const-exp)      
      (expression ("-" "(" expression "," expression ")")  diff-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression ("if" expression "then" expression "else" expression)  if-exp)
      (expression ("(" expression expression ")") call-exp)

      (expression ("pack" "(" (separated-list expression ",") ")" ) pack-exp)

      ;; names : identifier
      (expression (identifier) var-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)
      (expression ("proc" "(" identifier ")" expression) proc-exp)

      (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
      
      ;; Nameless
      (expression ("%letref" number) nameless-var-exp)
      (expression ("%let" expression "in" expression) nameless-let-exp)
      (expression ("%lexproc" expression) nameless-proc-exp)

      (expression ("%unpack" expression "in" expression) nameless-unpack-exp)
      ))
      
  ;;================================================================== SLLGEN
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  (define sp scan&parse)
  
  )
