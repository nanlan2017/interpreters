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
    '((Program ((arbno ModuleDefinition) Expression) $a-program)
      ;-------------- Type  --------------
      (Type ("int") $int-type)      
      (Type ("bool") $bool-type)      
      (Type ("(" Type "->" Type ")") $proc-type)
      ;-------------- Expression  --------------
      (Expression (number) $const-exp)
      (Expression (identifier) $var-exp)
      (Expression ("-" "(" Expression "," Expression ")")  $diff-exp)
      (Expression ("zero?" "(" Expression ")") $zero?-exp)
      (Expression ("if" Expression "then" Expression "else" Expression)  $if-exp)
      (Expression ("let" identifier "=" Expression "in" Expression) $let-exp)
      (Expression ("proc" "(" identifier ":" Type ")" Expression) $proc-exp)
      (Expression ("(" Expression Expression ")") $call-exp)
      (Expression ("letrec" Type identifier "(" identifier ":" Type ")" "=" Expression "in" Expression) $letrec-exp)      
      ;-------------- Module --------------
      (ModuleDefinition ("module" identifier "interface" SimpleInterface "body" ModuleBody) $a-module-definition)
      ; 定义
      (ModuleBody ("[" (arbno VarDefinition) "]") $a-module-body)
      (VarDefinition (identifier "=" Expression) $a-var-definition)
      ; 声明
      (SimpleInterface ("[" (arbno VarDeclaration) "]") $a-simple-interface)
      (VarDeclaration (identifier ":" Type) $a-var-declaration)            
      ; --------------
      (Expression ("from" identifier "take" identifier) $qualified-var-exp)     
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
