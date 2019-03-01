(module testcases (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  ; =========================================================
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
  ;; 【Proc】  
  (define src-proc-0     ; -> 55
    "let f = proc (x) -(x,11)
     in (f (f 77))"
    )  
  (define src-proc-1     ; -> 177   匿名函数
    "(proc (f) (f (f 199))   
      proc (x) -(x,11))"
    )
  (define src-proc-3
    "proc (x) -(x,3)"
    )

  
  ;;=========================================================== Error src
  (define sx-0
    "if zero? then x"
    )
  )























