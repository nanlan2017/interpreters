(module test-letrec-cps (lib "eopl.ss" "eopl")
  (provide (all-defined-out))  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  ; ==========================================================
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
  ;```````````````````````````````````````````````````````````
  ;; 【Proc】  
  (define src-proc-0     ; -> 55
    "let f = proc (x) -(x,11)
     in (f (f 77))"
    )  
  (define src-proc-1     ; -> 177   匿名函数
    "(proc (f) (f (f 199))   
      proc (x) -(x,11))"
    )
  (define src-proc-2
    "proc (x) -(x,3)"
    )
  (define src-proc-3
    "let dec = proc (x) -(x,1) in (dec 5)"  ; rator : 求dec是可以的[dec -> ProcVal] env
    )
;  (define src-proc-4
;    "letproc dec = proc (x) -(x,1) in (dec 56)"
;    )
  (define src-proc-5                        ; 自然支持curry (我们的算法本就是recursive的,只要每种ExpVal对的上位置就行)
    "let f = proc (x) proc (y) -(x,y)
     in ((f 33) 4)"
    )
  ; letproc
  (define src-proc-6
    "letproc f = proc (x) proc (y) -(x,y)
     in ((f 33) 4)"
    )
  ; 0/N 参数Proc
  (define src-proc-7
    "let f = proc () -(-(x,1) , 7)
     in (f)"
    )
  (define src-proc-8
    "let f = proc (x y z) -(-(x,y) , z)
     in (f 88 x 1)"
    )

  )
