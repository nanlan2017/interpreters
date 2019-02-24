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
      (Program ((arbno ClassDecl) Expression) $a-program)

      (Expression (number) $const-exp)
      (Expression (identifier) $var-exp)      
      (Expression ("-" "(" Expression "," Expression ")") $diff-exp)
      (Expression ("+" "(" Expression "," Expression ")") $sum-exp)      
      (Expression ("zero?" "(" Expression ")") $zero?-exp)
      (Expression ("if" Expression "then" Expression "else" Expression)  $if-exp)
      (Expression ("let" (arbno identifier "=" Expression) "in" Expression) $let-exp)   
      (Expression ("proc" "(" (separated-list identifier ":" Type ",") ")" Expression) $proc-exp)
      (Expression ("(" Expression (arbno Expression) ")") $call-exp)
      (Expression ("letrec" (arbno Type identifier "(" (separated-list identifier ":" Type ",") ")" "=" Expression) "in" Expression) $letrec-exp)      
      (Expression ("begin" Expression (arbno ";" Expression) "end") $begin-exp)
      (Expression ("set" identifier "=" Expression) $assign-exp)
      (Expression ("list" "(" Expression (arbno "," Expression) ")" ) $list-exp)

      ;;---------------------------------------------------------------------------- oop
      (ClassDecl                         
       ("class" identifier "extends" identifier (arbno "implements" identifier)
                (arbno "field" Type identifier)
                (arbno MethodDecl)
                )
       $a-class-decl)

      (MethodDecl
       ("method" Type identifier "("  (separated-list identifier  ":" Type ",") ")"
                 Expression
                 )
       $a-method-decl)

      ; 新增 4种Expression
      (Expression ("new" identifier "(" (separated-list Expression ",") ")") $new-object-exp)
      (Expression ("self") $self-exp)
      (Expression ("send" Expression identifier "("  (separated-list Expression ",") ")") $method-call-exp)
      (Expression ("super" identifier "(" (separated-list Expression ",") ")") $super-call-exp)

      ;;---------------------------------------------------------------------------- typed-oo

      (ClassDecl
       ("interface" identifier (arbno AbstractMethodDecl))
       $an-interface-decl)

      (AbstractMethodDecl
       ("method" Type identifier "("  (separated-list identifier ":" Type ",") ")" )
       $an-abstract-method-decl)

      ; 新增 2种Expression
      (Expression ("cast" Expression identifier) $cast-exp)
      (Expression  ("instanceof" Expression identifier) $instanceof-exp)

      ;; -------------------------------------------------------------------------- type
      (Type ("int") $int-type)             
      (Type ("bool") $bool-type)      
      (Type ("void") $void-type)      
      (Type ("(" (separated-list Type "*") "->" Type ")") $proc-type)
      (Type ("listof" Type) $list-type)
      (Type (identifier) $class-type) ;; new for typed oo

      ))

  ; ================================================================== SLLGEN  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

  (define sp scan&parse)

  
  )

