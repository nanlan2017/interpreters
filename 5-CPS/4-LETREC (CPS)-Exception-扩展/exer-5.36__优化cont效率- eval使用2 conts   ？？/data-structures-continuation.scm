(module data-structures-continuation (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;;============================================================ Continuation
  ; Continuation就是数据结构化的 CPS 中的后续函数！  
  ;      (if e1 then e2 else e3) #cont
  ;           eval e1 env \v1->
  ;               if v1 then eval e1 env #cont
  ;                     else eval e2 env #cont




  
  ;                    [一个典型的 try语句 的 cont 状态]
  ;  (value-of/k
  ;     <<raise 99>> ———— ———— ———— ———— ———— ———— ———— ———— ———— ——— 
  ;     ρlst=(())                                                    |
  ;     #(struct:diff1-cont <<-1>> ρlst=(3)                          |
  ;                       #(struct:diff1-cont <<-1>> ρlst=(2 3)      |
  ;                                           #(struct:try-cont x <<-(x,1)>> ρlst=(2 3)
  ;                                                             #(struct:end-cont)))))
  
  ; 但是采用了 procedural-rep (under  apply-cont)
  (define-datatype Continuation Continuation?
    ($end-cont)
    
    ; ░░░░░░░░░░░░░░░░ try-cont 保存起来纯粹是为了后面raise被求值时“回溯”用的，而不是像 if-exp 那样用于下一步计算！
    ;                   其实就相当于在 cont 栈中插入了一个 tag !
    ;                   运行时的continuations 表现为栈、就像 function chain一样
    ; Exception
    ($try-cont 
     (cvar identifier?)
     (handler-exp expression?)
     (env Env?)      ; [$Procedure (\cvar -> handler-exp) env]
     (cont Continuation?)
     )
    ($raise-cont            ; raise-cont 就是对 raise的值进行“回溯”
     (with-try-cont Continuation?)
     )
    ;````````````````````````````````
    ; unary : zero? | null? car cdr
    ($unary-arg-cont
     (op unary-op?)
     (cont Continuation?)
     (with-try-cont Continuation?)
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
     (with-try-cont Continuation?)
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
