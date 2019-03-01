#lang eopl
(provide (all-defined-out))
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(define the-lexical-spec
  '((whitespace (whitespace)
                skip)
      
    (comment ("%" (arbno (not #\newline)))
             skip)
    
    (emptylist ("emptylist")
               symbol)
    (identifier (letter (arbno (or letter digit "_" "-" "?")))
                symbol)
      
    (number (digit (arbno digit))
            number)
    (number ("-" digit (arbno digit))
            number)
 
    ))

;; 6种表达式： const-exp , zero? , diff-exp ,  if-exp , var-exp, let-exp
(define the-grammar
  '(
    ;; Program
    (program (expression)
             a-program)
    (program (Bool-exp)
             b-program)
    (program (Print-exp)
             prt-program)
    ;---------------------------------------------------------------------
    ;; Expression
    (expression (number)   ; 4
                const-exp)
    (expression (identifier)  ; x
                var-exp)
    (expression ("-" "(" expression "," expression ")") ; -(e1,e2)
                diff-exp)
    
    (expression ("if" Bool-exp "then" expression "else" expression)  ; if e1 then e2 else e3
                if-exp)
    (expression ("let" identifier "=" expression "in" expression)  ; let i = e1 in e2
                let-exp)

    (expression ("minus" "(" expression ")") ; minus(e1)
                minus-exp)
    (expression ("+" "(" expression "," expression ")")  ; + (e1,e2)
                add-exp)
    (expression ("*" "(" expression "," expression ")") ; * (e1,e2)
                mult-exp)
    (expression ("/" "(" expression "," expression ")") ; / (e1,e2)
                quotient-exp)
    ; (= e1 e2)           // 比较其num val 即可
    ; (> e1 e2)
    ; (< e1 e2)

    ; cons, car, cdr , null?
    (expression (emptylist)
                empty-list-exp)
    (expression ("cons" "(" expression "," expression ")")
                cons-exp)
    (expression ("car" "(" expression ")")
                car-exp)
    (expression ("cdr" "(" expression ")")
                cdr-exp)
    (expression ("null?" "(" expression ")")
                null?-exp)
    ; list(e1,e2,...)
    (expression ("list" "(" (separated-list expression ",") ")")
                list-exp)

    ; cond {Exp ==> Exp}* end
    ; cond (te1 ==> ev1) (te2 ==> ev2) end
    (expression ("cond" (arbno "(" Bool-exp "==>" expression ")") "end")
                cond-exp)
    ;---------------------------------------------------------------------------
    ; Bool-exp := 产生一个Bool值的表达式有哪些？
    (Bool-exp ("#t")
              const-true)
    (Bool-exp ("#f")
              const-false)
    (Bool-exp ("zero?" "(" expression ")")            ; zero? (e1)
              zero?-exp)
    ;---------------------------------------------------------------------------
    (Print-exp ("(" "print" expression ")")
               a-print-exp)
    ))
       
;;======================================================== SLLGEN
(sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
  
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
;;======================================================== 测试snippets
"x"
  
