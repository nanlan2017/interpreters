(module testcases (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "lang.scm")
  (require "data-structures.scm")
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
  (define src-proc-4
    "letproc dec = proc (x) -(x,1) in (dec 56)"
    )
  (define src-proc-5                        ; 自然支持curry (我们的算法本就是recursive的，只要每种ExpVal对的上位置就行)
    "let f = proc (x) proc (y) -(x,y)
     in ((f 33) 4)"
    )
  (define src-proc-6
    "letproc f = proc (x) proc (y) -(x,y)
     in ((f 33) 4)"
    )
  (define src-proc-7
    "let f = proc () -(-(x,1) , 7)
     in (f)"
    )
  (define src-proc-8
    "let f = proc (x y z) -(-(x,y) , z)
     in (f 88 x 1)"
    )
  ;; 柯里化
  (define src-proc-9      
    "let f = proc (x y z) -(-(x,y) , z)
     in let g = (f 88 x)        % g is curried function
        in (g 1)"
    )
  ;```````````````````````````````````````````````````````````
  ;; 递归
  (define src-rec-0            ;  由 (genf genf) 得到 f
    ;  mult4 x =  if x==0 then 0 else 4 + mult4 (x-1)   // mult4 是递归实现的
    "let makemult = proc (maker)
                      proc (x) if zero?(x) then 0 else -(((maker maker) -(x,1)), -4)
     in let times4 = proc (x) ((makemult makemult) x)
        in (times4 3)"
    )

  ;  let even = proc (x) if zero?(x) then 1 else (odd -(x,1))
  ;                                   odd -(x,1)  ==> if zero?(-(x,1)) then 0 else (even -(-(x,1),1))
  ;  let even = proc (x)      if zero?(x) then 1 else if zero?(-(x,1)) then 0 else (even -(-(x,1),1))
  ;  let evenGen = proc (Self x) if zero?(x) then 1 else if zero?(-(x,1)) then 0 else ((Self Self) -(-(x,1),1))
  ;  let oddGen  = proc (Self x) if zero?(x) then 0 else if zero?(-(x,1)) then 1 else ((Self Self) -(-(x,1),1))
  
  (define src-rec-1
    ; Error : 求值even时，odd还未出现
    "let even = proc (x) if zero?(x) then 1 else (odd -(x,1))
     in let odd = proc (x) 
        in (odd 13)"
    )
  ;; even-偶 : 偶数返回1，奇数返回0
  (define src-rec-3
    "let evenGen = proc (Self x) if zero?(x) then 1 else if zero?(-(x,1)) then 0 else ((Self Self) -(-(x,1),1))
     in let even = (evenGen evenGen)   % 支持函数赋值。因为我已经实现了柯里化
        in (even 7)"
    )

  (define (@even x)
    (if (= 0 x)
        1
        (if (= 0 (- x 1))
            0
            (@even (- x 2)))))
  
  (define src-rec-4
    "   let even-iter = proc (o e num) if zero?(num) then 1 else (((o o) e) -(num, 1))
     in let odd-iter  = proc (o e num) if zero?(num) then 0 else (((e o) e) -(num, 1))
           in let odd  = proc(num) (((odd-iter odd-iter) even-iter) num)
           in let even = proc(num) (((even-iter odd-iter) even-iter) num)
              in (odd 7)"
    )

  ;; Y combinator
  (define src-rec-5
    "
  let makerec = proc (f)
let d = proc (x)
proc (z) ((f (x x)) z)
in proc (n) ((f (d d)) n)
in let maketimes4 = proc (f)
proc (x)
if zero?(x)
then 0
else -((f -(x,1)), -4)
in let times4 = (makerec maketimes4)  % Y组合子： f = (Y fGen)
in (times4 3)
"
    )
  ;```````````````````````````````````````````````````````````
  (define src-rec-6              ;; 不行：暂不支持定义递归函数
    "let fact = proc (n) if zero?(n) then 1 else *(n,(fact -(n,1)))
     in (fact 5)"
    )
  (define src-rec-7     ; -> 720
    "letrec fact(n) = if zero?(n) then 1 else *(n,(fact -(n,1)))
     in (fact 6)"
    )
  ;```````````````````````````````````````````````````````````
  
  ;;=========================================================== run
  (define (run-tests)
    (begin 
      (interp src0-0)
      (interp src0-1)
      (interp src0-2)
      (interp src0-3)
      (interp src0-4)
      (interp src0-5)
      ;; proc
      (interp src-proc-0)
      (interp src-proc-1)
      ;; recursive
      (interp src-rec-6)  ; wrong case
      (interp src-rec-7)
      ))

  ;  (run-tests)

  ;; short names
  (define sp scan&parse)
  (define run interp)
  
  ;;=========================================================== Error src
  (define sx-0
    "if zero? then x"
    )
  )























