(module testcases (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang-in-out.scm")
  (require "transform-CPS-IN-to-CPS-OUT.scm")
  (require "pprinter-CPS-OUT.scm")

  
  ;██████ CPS-of-exp  【就是我们的手动将 Expression Monad化、改写为Tail Form的过程啊！】


  ;-----------------------------------------------------------  ░░░░ call-exp  ===> cps-call-exp
;  手动CPS:
;  (p (+ 8 x) (q y))
;  =====>
;  (q y (λ (v2)
;         (p (+ 8 x) v2 (λ (v1)
;                         v1))))
;  
;  (p +(8,x) (q y))
;  =====>
;  (q y (proc (v%2)
;             (p +(8,x) var%2 (proc (v%1)
;                                   v%1))))

  (rewrite "(p +(8,x) (q y))")


  "+(37, if zero?(x) then (f y) else (g z))"
;if zero?(x)
;  then (f y proc(v%2) => (proc(v%1) => v%1 +(37, v%2)))
;  else (g z proc(v%2) => (proc(v%1) => v%1 +(37, v%2)))
                    
  ;-----------------------------------------------------------
  )
