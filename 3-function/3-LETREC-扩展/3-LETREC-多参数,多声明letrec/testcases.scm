(module testcases (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  ;;=========================================================== (init-env) : [x=1, v=5, x=10]

  (define src1
    "let minus = proc (x y) -(x,y)
     in (minus 100 77)
")
  (define src2
  "
  letrec
  even(x) = if zero?(x) then 1 else (odd -(x,1))
  odd(x)  = if zero?(x) then 0 else (even -(x,1))
  in (odd 13)
")

  (run "letrec double(x)
            = if zero?(x) then 0 else -((double -(x,1)), -2)
       in (double 6)")
  )























