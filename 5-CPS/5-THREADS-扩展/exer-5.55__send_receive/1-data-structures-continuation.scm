(module data-structures-continuation (lib "eopl.ss" "eopl")
  (require "0-lang.scm")
  (require "0-store.scm")
  (require "1-data-structures.scm")

  (provide (all-defined-out))
  ; ======================================================================== continuations
  (define-datatype Continuation Continuation?
    
    ($end-cont-main-thread)      
    
    ($end-cont-subthread)        ; spawn(p) 创建的线程就是 sub-thread

    ; 多线程
    ($spawn-cont 
     (saved-cont Continuation?))
    
    ($wait-cont 
     (saved-cont Continuation?))
    
    ($signal-cont 
     (saved-cont Continuation?))
    ;;
    ($send1-cont
     (exp2 expression?)
     (env Env?)
     (saved-cont Continuation?))
    ($send2-cont
     (thread-id-val ExpVal?)
     (saved-cont Continuation?))

    ;````````````````````````````````
    ; diff
    ($diff1-cont               
     (exp2 expression?)
     (env Env?)
     (cont Continuation?))
    ($diff2-cont        
     (val1 ExpVal?)
     (cont Continuation?))
    ; if
    ($if-test-cont
     (exp2 expression?)
     (exp3 expression?)
     (env Env?)
     (cont Continuation?))
    ; unary-op
    ($unop-arg-cont
     (unop1 unop?)
     (cont Continuation?))
    ; call
    ($rator-cont        
     (rand expression?)
     (env Env?)
     (cont Continuation?))
    ($rand-cont                        
     (val1 ExpVal?)
     (cont Continuation?))
    ; set
    ($set-rhs-cont
     (loc reference?)
     (cont Continuation?))
    )
  )
