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
      (program (statement) a-program)
      ;----------------- statement --------------------------------------
      (statement (identifier "=" expression) assign-stat)
      (statement ("print" expression) print-stat)
      (statement ("{" (separated-list statement ";") "}") seq-stat)
      (statement ("if" expression "then" statement "else" statement)  if-stat)
      (statement ("while" expression statement) while-stat)
      (statement ("var" (separated-list identifier ",") ";" statement) block-stat)
      ;----------------- expression -------------------------------------
      (expression (number) const-exp)
      (expression (identifier) var-exp)

      (expression ("-" "(" expression "," expression ")")  diff-exp)
      (expression ("+" "(" expression "," expression ")") add-exp)
      (expression ("*" "(" expression "," expression ")") mult-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression ("if" expression "then" expression "else" expression)  if-exp)
      (expression ("not" "(" expression ")") not-exp)
      
      (expression ("let" identifier "=" expression "in" expression) let-exp)
      (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression) letrec-exp)

      (expression ("proc" "(" identifier ")" expression) proc-exp)
      (expression ("(" expression expression")") call-exp)

      (expression ("begin" (separated-list expression ";") "end") begin-exp)
      (expression ("set" identifier "=" expression) assign-exp)
      ))
      
  ;;================================================================== SLLGEN
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))    
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  (define sp scan&parse)
  
  )
