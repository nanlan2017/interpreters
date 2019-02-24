(module front1 (lib "eopl.ss" "eopl")


  (define the-lexical-spec
    '(
      ;; (tokenType RegExp Action)
      
      ;; 支持原语：  arbno(*) /  letter,digit,whitespace  / or,not
      ;;             skip / 
      (whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      
      ))

;  (data program = a-program expression)
;  (data expression = const-exp number
;                    |var-exp identifer
;                    |fuck-exp expression expression)
  (define the-grammar
    '(
      ;; (nodeType Production Variants)

      (program (expression) a-program)
      (expression (number) const-exp)
      (expression (identifier) var-exp)
      (expression ("fuck" expression expression) fuck-exp)
      ))
      
  ;;======================================================
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

  ;;------------------------
;  (define (print-tokens text)
;    (let [(list (just-scan text))]
;      (map (λ 

  ;;======================================================
  (define 
  (define src0
    "% this is comment
fuck it 235")

  )



























