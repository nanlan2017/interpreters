(module testcases (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "0-lang.scm")
  (require "1-typer.scm")
  (require "2-interp.scm")
  ; =========================================================================

  (define src-0
    "
proc (x ) -(x,1)            %%%%%%%% proc(x:t0) -(x,1) , 求t0->int
"
    )
  (define src-1
    "
letrec double (x : int) = if zero?(x)
                               then 0
                               else -((double -(x,1)), -2)
in double
")
  (define src-2
    "
proc (f : (bool -> int)) proc (n : int) (f zero?(n))
"
    )
  ;===================================== 多态类型
  (define src-3
    "
proc (x) x
"
    )

  ;===================================== ill-typed
  (define srcx-1
    "
proc (x ) -(x,zero?(v))
"
    )

  )
