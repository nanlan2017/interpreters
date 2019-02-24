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
letrec
int double (x : int) = if zero?(x)
then 0
else -((double -(x,1)), -2)
in double
")
  (define src-2
    "
proc (f : (bool -> int)) proc (n : int) (f zero?(n))
"
    )
  ; ================================================== pair ,list
  (define src-pl-1
    "
let p1 = newpair(3,zero?(0))
in unpair a b = p1
   in b
"
    )
  (define src-pl-2
    "
let id = proc(x:int) x
in let lst1 = list(3,5,6,[int])
   in lst1
"
    )

  )
