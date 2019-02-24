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

    ($kill-cont 
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

  ; ------------------------------------------------------------------------------------
  ; :: Continuation -> String
  (define (get-cont-variant-name k)
    (cases Continuation k
      ($end-cont-main-thread ()
                             "$end-cont-main-thread")
      ($end-cont-subthread ()
                           "$end-cont-subthread")         
      ($spawn-cont (cont)
                   "$spawn-cont")

      ($wait-cont (cont)
                  "$wait-cont")

      ($signal-cont (cont)
                    "$signal-cont")
      ; unary-op
      ($unop-arg-cont (unop1 cont)
                      "$unop-arg-cont")
      ; diff-exp
      ($diff1-cont (exp2 saved-env saved-cont)
                   "$diff1-cont")
      ($diff2-cont (val1 saved-cont)
                   "$diff2-cont")
      ; if
      ($if-test-cont (exp2 exp3 env cont)
                     "$if-test-cont")
      ; call-exp
      ($rator-cont (rand saved-env saved-cont)
                   "$rator-cont")
      ($rand-cont (val1 saved-cont)
                  "$rand-cont")
      ; set
      ($set-rhs-cont (loc cont)
                     "$set-rhs-cont")
      (else "xx-cont")
      ))

  ; :: Expression -> String
  (define (get-exp-variant-name exp)     
    (cases expression exp
      (const-exp (num)
                 "const-exp")
      (var-exp (var)
               "var-exp")
      (proc-exp (var body)
                "proc-exp")
      (let-exp (var exp1 body)
               "let-exp")
      (letrec-exp (p-names b-vars p-bodies letrec-body)
                  "letrec-exp")
      (const-list-exp (nums)
                      "const-list-exp")
      (diff-exp (exp1 exp2)
                "diff-exp")
      (if-exp (exp1 exp2 exp3)
              "if-exp")
      (call-exp (rator rand)
                "call-exp")
      (begin-exp (exp exps)    
                 "begin-exp")
      (set-exp (id exp)
               "set-exp")
      (unop-exp (unop1 exp)
                "unop-exp")
      (spawn-exp (exp)
                 "spawn-exp")
      (mutex-exp ()
                 "mutex-exp")
      (wait-exp (exp)
                "wait-exp")
      (signal-exp (exp)
                  "signal-exp")
      (yield-exp ()
                 "yield-exp")
      (else "xx-exp")
      ))

  )
