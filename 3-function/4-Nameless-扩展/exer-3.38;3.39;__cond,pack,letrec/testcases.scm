(module testcases (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  ;;=========================================================== (init-env) : [i=1, v=5, x=10]
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
  
  ;```````````````````````````````````````````````````````````` nameless 
  (define src-nl-0
    "let x = 37
     in proc (y)
        let z = -(y,x)
        in -(x,y)"
    )
  ;```````````````````````````````````````````````````````````` pack ,unpack
  (define src-pack-1
    "
  let lst = pack(i,x)
  in unpack a b = lst
     in pack(b,a)
  "
    )

  )























  