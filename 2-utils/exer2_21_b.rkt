#lang eopl

(define-datatype env-type env?
  (empty-env)
  (extend-env
   (var symbol?)
   (val always?)
   (env env?)))

(define apply-env
  (lambda (env search-var)
    (cases env-type env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-var)]
      [extend-env (var val env) (if (eqv? var search-var)
                                    val
                                    (apply-env env search-var))])))

(define has-binding?
  (lambda (env search-var)
    (cases env-type env
      [empty-env () #f]
      [extend-env (var val env) (or (eqv? var search-var)
                                    (has-binding? env search-var))])))

;;======================================
(define (not-lambda? sym)
  (and (symbol? sym)
       (not (eqv? 'lambda sym))))

(define-datatype type-lc-exp lc-exp?
  (var-exp
   (var symbol?))

  (lambda-exp
   (bound-var not-lambda?)
   (body lc-exp?))

  (app-exp
   (exp1 lc-exp?)
   (exp2 lc-exp?)))

(define expr1 (lambda-exp 'x (var-exp 'x)))
;(define expr2 (lambda-exp 'lambda (var-exp 'x)))
;;======================================
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define src0 '(- - 3 2 - 4 - 12 7))
(define src1 '(2))
(define src2 '(- 2 1))


;; 既然是写递归，那么就要假定我这个rec 函数就是能 正确地计算出一个 prefix-exp 
(define parse-prefix-exp
  (lambda (prefix-list)
    (let ([head (car prefix-list)]
          [tail (cdr prefix-list)])
      (cond [(integer? head) (cons (const-exp head) tail)]
            [(eqv? head '-) (let* ([operand-1-and-rest-1 (parse-prefix-exp tail)]
                                   [operand-1 (car operand-1-and-rest-1)]
                                   [rest-1 (cdr operand-1-and-rest-1)]
                                   
                                   [operand-2-and-rest-2 (parse-prefix-exp rest-1)]
                                   [operand-2 (car operand-2-and-rest-2)]
                                   [rest-2 (cdr operand-2-and-rest-2)])
                              
                              (cons (diff-exp operand-1 operand-2) rest-2))]
            [else (eopl:error 'parse-prefix-exp "Bad syntax: ~s." prefix-list)]))))

(define parse-prefix-list
  (lambda (prefix-list)
    (let* ([exp-and-rest (parse-prefix-exp prefix-list)]
           [exp (car exp-and-rest)]
           [rest (cdr exp-and-rest)])
      (if (null? rest)
          exp
          (eopl:error 'parse-prefix-list "Expect null after prefix-exp, but got: ~s." rest)))))

                
(parse-prefix-exp src0)
(parse-prefix-exp src1)
(parse-prefix-exp src2)


















