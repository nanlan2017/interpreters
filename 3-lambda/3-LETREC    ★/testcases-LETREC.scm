(module testcases-LETREC (lib "eopl.ss" "eopl")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  
  (provide (all-defined-out))
  ; ==============================================================================

  (define src-0
  "
  letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2)
  in (double 6)
")

  (define src-rec-7     ; -> 720
    "letrec fact(n) = if zero?(n) then 1 else *(n,(fact -(n,1)))
     in (fact 6)"
    )



  )























