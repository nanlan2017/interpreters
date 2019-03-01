#lang eopl
(provide (all-defined-out))
;;====================================== Expressed Value | Denoted Value
;; ExpVal 应该能表示所有expression的可能的值
(define-datatype ExpVal ExpVal?
  ($num-val
   (num number?))
  
  ($bool-val
   (v boolean?))

  ($emptylist)   ; 无法用$cons-val来表示一个 空表的值
  ($cons-val
   (h ExpVal?)
   (t ExpVal?))
  )

;(define-datatype ListVal ListVal?
;  ($emptylist)
;  ($cons-val
;   (h ExpVal?)
;   (t ExpVal?)))

;; expval -> number
(define (expval->num expval)
  (cases ExpVal expval
    ($num-val (n) n)
    (else (eopl:error "Can't get num-val from this ExpVal : " expval))))
;; expval -> boolean
(define (expval->bool expval)
  (cases ExpVal expval    
    ($bool-val (b) b)
    (else (eopl:error "Can't get bool-val from num ExpVal : " expval))))
;; 取head部分
(define (get-head expval)
  (cases ExpVal expval
    ($cons-val (h t) h)
    (else (eopl:error "Can't get head-val from num ExpVal : " expval))))
;; 取tail部分
(define (get-tail expval)
  (cases ExpVal expval
    ($cons-val (h t) t)
    (else (eopl:error "Can't get tail-val from num ExpVal : " expval))))
;; 转为字面值list
(define (expval->list expval)
  (cases ExpVal expval
    ($emptylist ()
                '())
    ($cons-val (hval tvals)
               (let [(h (cases ExpVal hval
                          ($bool-val (b) b)
                          ($num-val (n) n)
                          (else (expval->list hval))))]
                 (cons h (expval->list tvals))))
    (else (eopl:error "Can't get list-val from num ExpVal : " expval))))
               
;;====================================== Env (采用datatype 表示法)
(define-datatype Env Env?
  ($empty-env)
    
  ($extend-env
   (var symbol?)
   (val ExpVal?)  ;注意：这里的val类型为ExpVal
   (env Env?)))

(define (init-env)
  ($extend-env 'i ($num-val 1)
               ($extend-env 'v ($num-val 5)
                            ($extend-env 'x ($num-val 10)
                                         ($empty-env)))))

(define (apply-env env var)
  (cases Env env
    ($empty-env ()
                (eopl:error 'apply-env "Empty env while search ~s" var))
    ($extend-env (saved-var saved-val saved-env)
                 (if (eqv? saved-var var)
                     saved-val
                     (apply-env saved-env var)))))
  
