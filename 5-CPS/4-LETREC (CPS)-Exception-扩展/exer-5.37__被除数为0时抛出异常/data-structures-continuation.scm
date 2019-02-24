(module data-structures-continuation (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;;============================================================ Continuation
  (define-datatype Continuation Continuation?
    ($end-cont)

    ; Exception
    ($try-cont
     (cvar identifier?)
     (handler-exp expression?)
     (env Env?)
     (cont Continuation?))
    ($raise1-cont
     (cont Continuation?))
    ;````````````````````````````````
    ; unary : zero? | null? car cdr
    ($unary-arg-cont
     (op unary-op?)
     (cont Continuation?))
    
    ; if-exp
    ($if-test-cont
     (then-exp expression?)
     (else-exp expression?)
     (env Env?)
     (cont Continuation?))
    
    ; diff-exp
    ($diff1-cont
     (e2 expression?)
     (env Env?)
     (cont Continuation?))
    ($diff2-cont
     (v1 ExpVal?)
     (cont Continuation?))
    
    ; call-exp
    ($rator-cont
     (rand-exp-s (list-of expression?))
     (env Env?)
     (cont Continuation?))
    ($rands-cont
     (f Proc?)
     (randvalss (list-of ExpVal?))
     (rand-exp-s (list-of expression?))
     (env Env?)
     (cont Continuation?))
    )
  )
