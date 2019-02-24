(module lang (lib "eopl.ss" "eopl")       
  (provide (all-defined-out))
  ; ===================================================================
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)
      ; ---------------------- expression ------------------------------
      (expression (number) const-exp)           
      (expression (identifier) var-exp)   
      (expression ("-" "(" expression "," expression ")") diff-exp)
      (expression ("if" expression "then" expression "else" expression) if-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)
      ; 1-arg 
      (expression ("proc" "(" identifier ")" expression) proc-exp)
      (expression ("(" expression expression ")") call-exp)
      
      (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression) letrec-exp)
      (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
      (expression ("set" identifier "=" expression) set-exp)
      (expression (unop "(" expression ")") unop-exp)
      ;  unary operators             
      (unop ("car") car-unop)
      (unop ("cdr") cdr-unop)
      (unop ("null?") null?-unop)
      (unop ("zero?") zero?-unop)
      (unop ("print") print-unop)      
      ; [1,2,3]
      (expression ("[" (separated-list number ",") "]") const-list-exp)
      
      ; threads 
      (expression ("spawn" "(" expression ")") spawn-exp)
      
      (expression ("mutex" "(" ")") mutex-exp)  ; new-mutex
      (expression ("wait" "(" expression ")") wait-exp)      
      (expression ("signal" "(" expression ")") signal-exp)
      
      (expression ("yield" "(" ")") yield-exp)

      (expression ("kill" "(" expression ")") kill-exp)
      
      ))

  ; ================================================================ 
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))  
  (define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))
  (define sp scan&parse)
  
  )
