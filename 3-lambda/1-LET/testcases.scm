(module testcases (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "lang.scm")
  (require "interp.scm")
  ;;=========================================================== (init-env) : [x=1, v=5, x=10]
  (define src0-0
    "3"
    )
  (define src0-1
    "x"
    )
  (define src0-2
    "-(x,3)"
    )
  (define src0-3
    "zero?(v)"
    )
  (define src0-4
    "let y = 5 in -(x,y)"
    )
  (define src0-5
    "if zero? (x) then i else -(v,x)"
    )
  ;;=========================================================== Error src
  (define srcx-0
    "if zero? then x"
    )



  ;;===========================================================
  (define (run-tests)
    (begin 
      (interp src0-0)
      (interp src0-1)
      (interp src0-2)
      (interp src0-3)
      (interp src0-4)
      (interp src0-5)))

  )























