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
      ;---------------------------- Type ----------------------------
      ; ::= Int | Bool | m::opat | Type -> Type
      (Type ("int") $int-type)      
      (Type ("bool") $bool-type)      
      (Type ("(" Type "->" Type ")") $proc-type)
      
      (Type (identifier) $named-type)                              ; Point               // 用于module内部
      (Type ("from" identifier "take" identifier) $qualified-type) ; from m1 take Point  // 用于module外部 (需要m1::限定)
      ;---------------------------- Expression ----------------------------
      (Expression (number) $const-exp)
      (Expression (identifier) $var-exp)
      (Expression ("-" "(" Expression "," Expression ")")  $diff-exp)
      (Expression ("zero?" "(" Expression ")") $zero?-exp)
      (Expression ("if" Expression "then" Expression "else" Expression)  $if-exp)
      (Expression ("let" identifier "=" Expression "in" Expression) $let-exp)      
      (Expression ("proc" "(" identifier ":" Type ")" Expression) $proc-exp)
      (Expression ("(" Expression Expression ")") $call-exp)      
      (Expression ("letrec" Type identifier "(" identifier ":" Type ")" "=" Expression "in" Expression) $letrec-exp)      
      ;---------------------------- Module ----------------------------
      (ModuleDefinition ("module" identifier "interface" Interface "body" ModuleBody) $a-module-definition)      
      ; 接口 ::=  [ Declarations ]
      (Interface ("[" (arbno VarDeclaration) "]") $simple-interface)
      (Interface ("(" "(" identifier ":" Interface ")" "=>" Interface ")") $proc-interface)
      
      (VarDeclaration ("opaque" identifier) $opaque-type-declaration)
      (VarDeclaration ("transparent" identifier "=" Type) $transparent-type-declaration)
      (VarDeclaration (identifier ":" Type) $a-var-declaration)
      ; 实现 ::=  [ Definitions ]
      (ModuleBody ("[" (arbno VarDefinition) "]") $a-module-body)
      (ModuleBody ("module-proc" "(" identifier ":" Interface ")" ModuleBody) $proc-module-body)  ; to-int-maker (ints)
      (ModuleBody (identifier) $var-module-body)                                                  ; ints
      (ModuleBody ("(" identifier identifier ")") $app-module-body)                               ; (to-int-maker ints1)

      (VarDefinition ("type" identifier "=" Type) $type-definition)
      (VarDefinition (identifier "=" Expression) $a-var-definition)
      ; ---------------
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
