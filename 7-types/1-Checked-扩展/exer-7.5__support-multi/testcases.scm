(module test-checker (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "type-checker.scm")
  (require "interp.scm")
  ; =========================================================================

  (define src-0
    "
proc (x : int) -(x,1)
"
    )
  (define src-1
    "
letrec int double (x : int) = if zero?(x) then 0 else -((double -(x,1)), -2)
in double
")
  (define src-2
    "
proc (f : (bool -> int)) proc (n : int) (f zero?(n))
"
    )
  ; =========================================================== exer-7.5  multi
  (define src-multi-0
    "
let a = 3 , b = 4 , 
    f = proc (x:int, y:bool) if y then x else 5
in (f a zero?(b))
")

  (define src-multi-1   ; letrec 前面只标注 res-type，而非整个 f 的type
    "
letrec int double (x : int, y:bool, z:int) = if zero?(x) then 0 else -((twice -(x,1) zero?(0) 3), -2)
       int twice (x : int, y:bool, z:int) = if zero?(x) then 0 else -((double -(x,1) zero?(0) 3), -2)
in -((double 20 zero?(0) 3),(twice 20 zero?(0) 3))
"
    )
  )
