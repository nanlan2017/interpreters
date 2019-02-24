(module lang (lib "eopl.ss" "eopl")

  ;; grammar for the CLASSES language.
  ;; Based on IMPLICIT-REFS, 
  ;; plus : multiple-argument procedures, multiple-declaration letrecs, and multiple-declaration lets.   
  (provide (all-defined-out))
  ; ======================================================================
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '(
      (Program ((arbno ClassDecl)
                Expression)
               $a-program)
      ; ------------------------ Expression ------------------------
      (Expression (number) $const-exp)
      (Expression (identifier) $var-exp)
      
      (Expression ("-" "(" Expression "," Expression ")") $diff-exp)
      (Expression ("+" "(" Expression "," Expression ")") $sum-exp)      
      (Expression ("zero?" "(" Expression ")") $zero?-exp)
      (Expression ("if" Expression "then" Expression "else" Expression) $if-exp)
      ; multi version
      (Expression ("let" (arbno identifier "=" Expression) "in" Expression) $let-exp)
      (Expression ("proc" "(" (separated-list identifier ",") ")" Expression) $proc-exp)
      (Expression ("(" Expression (arbno Expression) ")") $call-exp)           
      (Expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")"  "=" Expression)  "in" Expression) $letrec-exp)
      
      (Expression ("begin" Expression (arbno ";" Expression) "end") $begin-exp)
      (Expression ("set" identifier "=" Expression) $assign-exp)
      (Expression ("list" "(" (separated-list Expression ",") ")" ) $list-exp)

      ; ------------------------ OOP ------------------------
      (ClassDecl                         
       ("class" identifier "extends" identifier                   
                (arbno "field" identifier)
                (arbno MethodDecl)
                )
       $a-class-decl)

      (MethodDecl
       ("method" identifier "("  (separated-list identifier ",") ")"
                 Expression 
                 )
       $a-method-decl)

      (Expression ("new" identifier "(" (separated-list Expression ",") ")") $new-object-exp)
      (Expression ("self") $self-exp)
      (Expression ("send" Expression identifier "("  (separated-list Expression ",") ")") $method-call-exp)
      (Expression ("super" identifier "("  (separated-list Expression ",") ")") $super-call-exp)

      ))

  ; ================================================================== SLLGEN  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  (define sp scan&parse)
  
  )

