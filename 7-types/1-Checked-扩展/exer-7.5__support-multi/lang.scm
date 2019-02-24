(module lang (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)      
      (comment ("%" (arbno (not #\newline)))skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  ; (sp "(int * bool -> int)")
  ; -> 左侧一定要留一个空格
  (define the-grammar
    '(
      ; (signature (Type) $a-signature)
      (program (expression) $a-program)
      
      ;------------------------- Type -------------------------------------
      (Type ("int") $int-type)      
      (Type ("bool") $bool-type)      
      (Type ("(" (separated-list Type "*") "->" Type ")") $proc-type)
      ;------------------------- Expression -------------------------------
      (expression (number) $const-exp)
      (expression (identifier) $var-exp)
      (expression ("-" "(" expression "," expression ")")  $diff-exp)
      (expression ("zero?" "(" expression ")") $zero?-exp)
      (expression ("if" expression "then" expression "else" expression)  $if-exp)
      
      (expression ("let" (separated-list identifier "=" expression ",") "in" expression) $let-exp)

      (expression ("proc" "(" (separated-list identifier ":" Type ",") ")" expression) $proc-exp)
      (expression ("(" expression (arbno expression) ")") $call-exp)

      (expression ("letrec" (arbno Type identifier "(" (separated-list identifier ":" Type ",") ")" "=" expression) "in" expression) $letrec-exp)      
      ))
      
  ; ================================================================== SLLGEN
  (define identifier? symbol?)
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  (define sp scan&parse)
  
  )
