(module testcases-explicitref (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "0-lang.scm")
  (require "1-store.scm")
  (require "1-data-structures.scm")
  (require "2-interp.scm") 
  ;================================================================================

  (define src-1        ;; 共享变量用法 （调用参数不用传，直接约定为某个 global var)
    "
  let x = newref(0)
  in letrec even(dummy) = if zero?(deref(x)) then 1               % letrec* 尚未支持！
                                        else begin
                                               setref(x, -(deref(x),1)) ;
                                               (odd 999)
                                             end
            odd (dummy) = if zero?(deref(x)) then 0
                                        else begin
                                               setref(x, -(deref(x),1)) ;
                                               (even 999)
                                             end
     in begin
          setref(x,13) ;
          (odd 999)
        end
    ")

  (define src-2      ;; g:每次调用返回计数器+9  (带可变状态的闭包)
    "
  let g = let counter = newref(0)
          in proc (dummy)
               begin
                 setref(counter, -(deref(counter), -9));
                 deref(counter)
               end
  in let a = (g 11)
     in let b = (g 11)
        in -(a,b)
   ")
  
  )
