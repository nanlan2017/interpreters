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

  )
