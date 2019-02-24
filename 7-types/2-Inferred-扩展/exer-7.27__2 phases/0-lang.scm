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
      ;------------------------- Type -------------------------------------
      (Type ("int") $int-type)      
      (Type ("bool") $bool-type)      
      (Type ("(" Type "->" Type ")") $proc-type)
      
      ; Type var (t0,t1...)
      (Type ("%tvar-type" number) $tvar-type)
      
      ; optional type
      (OptionalType () $no-type)
      (OptionalType (":" Type) $a-param-type)
      (OptionalType (Type) $a-ret-type)
      ;------------------------- Expression -------------------------------
      (expression (number) const-exp)
      (expression (identifier) var-exp)
      (expression ("-" "(" expression "," expression ")")  diff-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression ("if" expression "then" expression "else" expression)  if-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)
      
      (expression ("proc" "(" identifier OptionalType ")" expression) proc-exp)
      (expression ("(" expression expression ")") call-exp)
      
      (expression ("letrec" OptionalType identifier "(" identifier OptionalType ")" "=" expression "in" expression) letrec-exp)      
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
