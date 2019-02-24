#lang racket

;;  ds-representation
;(define (empty-env) '(empty-env))
;
;(define (extend-env var val env)
;  (list 'extend-env var val env))
;
;(define (extend-env* var-list val-list env)
;  ;;  (x y z) (1 2 3) env   ===>  ('ex x 1 'ex y 2 'ex z 3 env)
;  (for 
;
;(define (apply-env env search-var)
;  (let [(flag (car env))]
;    (cond
;      [(eqv? flag 'empty-env) (error 'apply-env "No binding found for ~s" search-var)]
;      [(eqv? flag 'extend-env) (let [(saved-var (cadr env))
;                                     (saved-val (caddr env))
;                                     (saved-env (cadddr env))]
;                                 (if (eqv? saved-var search-var)
;                                     saved-val
;                                     (apply-env saved-env search-var)))]
;      [else (error 'apply-env "Bad environment:~s" env)])))



;;============================================================ Exer 2.5 a-list 表示

;;================================ procedural-presentation : 所有 env 均表示为  λ (var) (...)
(define (empty-env)
  (λ (var) #f))

(define (extend-env var val env)
  (λ (search-var)
    (if (eqv? search-var var)
        val
        (apply-env env search-var))))

(define (apply-env env search-var)
  (env search-var))


;------------------
(define env0 (extend-env 'x 1
                         (extend-env 'y 2
                                     (extend-env 'z 3 (empty-env)))))

(apply-env env0 'y)






























         