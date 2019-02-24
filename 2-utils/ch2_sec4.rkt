#lang racket
(require eopl)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))