(module data-structures-continuation (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  
  ; 但是采用了 procedural-rep (under  apply-cont)
  (define-datatype Continuation Continuation?
    ($end-cont)

    ; Exception
    ($try-cont 
     (cvar identifier?)
     (handler-exp expression?)
     (env Env?)             ; [$Procedure (\cvar -> handler-exp) env]
     (cont Continuation?)
     (with-try-cont Continuation?)
     )
    ($raise-cont
     (with-try-cont Continuation?)
     (resume-cont Continuation?)
     )
    ;````````````````````````````````
    ; unary : zero? | null? car cdr
    ($unary-arg-cont
     (op unary-op?)
     (cont Continuation?)
     )
    
    ; if-exp
    ($if-test-cont
     (then-exp expression?)
     (else-exp expression?)
     (env Env?)
     (cont Continuation?)
     (with-try-cont Continuation?)
     )
    
    ; diff-exp
    ($diff1-cont
     (e2 expression?)
     (env Env?)
     (cont Continuation?)
     (with-try-cont Continuation?)
     )
    ($diff2-cont
     (v1 ExpVal?)
     (cont Continuation?)
     )
    
    ; call-exp
    ($rator-cont
     (rand-exp expression?)
     (env Env?)
     (cont Continuation?)
     (with-try-cont Continuation?)
     )
    ($rand-cont
     (f-expval ExpVal?)
     (cont Continuation?)
     (with-try-cont Continuation?)
     )
    
    )
  )
