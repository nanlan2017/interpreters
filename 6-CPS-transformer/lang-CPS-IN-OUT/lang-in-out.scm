(module lang-CPS-IN (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (define lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define in-grammar
    '((program (expression) a-program)
      ; ----------- expression -------------------
      (expression (number) const-exp)
      (expression (identifier) var-exp)
      (expression ("-" "(" expression "," expression ")") diff-exp)      
      (expression ("+" "(" (separated-list expression ",") ")") sum-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression ("if" expression "then" expression "else" expression) if-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)     
      ; N-args-procedure
      (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
      (expression ("(" expression (arbno expression) ")") call-exp)
      ; multi-N-args-letrec
      (expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression) letrec-exp)
      ))
  ; ===============================================================================================================

  (define cps-out-grammar
    '((CpsProgram (TailExp) a-cps-program)
      ; ---------------- simple expression : never contains any procedure calls
      (SimpleExp (number) @const-exp)
      (SimpleExp (identifier) @var-exp)      
      (SimpleExp ("-" "(" SimpleExp "," SimpleExp ")") @diff-exp)
      (SimpleExp ("+" "(" (separated-list SimpleExp ",") ")") @sum-exp)
      (SimpleExp ("zero?" "(" SimpleExp ")") @zero?-exp)      
      (SimpleExp ("proc" "(" (arbno identifier) ")" TailExp) @proc-exp)
      ;----------------- tail-call expression : must in tail-form
      (TailExp (SimpleExp) $simple-exp->exp)
      (TailExp ("if" SimpleExp "then" TailExp "else" TailExp) $if-exp)
      (TailExp ("let" identifier "=" SimpleExp "in" TailExp) $let-exp)   
      (TailExp ("(" SimpleExp (arbno SimpleExp) ")")  $call-exp)
      (TailExp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" TailExp) "in" TailExp) $letrec-exp)
      ))

  ; ===============================================================================================================
  
  (sllgen:make-define-datatypes lexical-spec in-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes lexical-spec in-grammar)))  
  (define scan&parse
    (sllgen:make-string-parser lexical-spec in-grammar))  
  (define sp scan&parse)
  ; ----------------------
  (sllgen:make-define-datatypes lexical-spec cps-out-grammar)  
  (define cps-show-the-datatypes
    (lambda ()
      (sllgen:list-define-datatypes lexical-spec cps-out-grammar))) 
  (define cps-out-scan&parse
    (sllgen:make-string-parser lexical-spec cps-out-grammar))  
  (define cps-sp cps-out-scan&parse)
  
  )
