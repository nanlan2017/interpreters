(module test-exception (lib "eopl.ss" "eopl")
  (provide (all-defined-out))  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  ; ==========================================================

  (define src-0
    "raise 1"
    )

  (define src-1
    "
try
  let y = 4
  in raise 101
catch(e)
  e
")

  (define src-2
    "
try
  let y = 4
  in raise -(101,y)
catch(e)
  e
")


  (define src-rec
    "
letrec sum (lst) =
  if null?(lst) then 0
                else -(car(lst),-(0,(sum cdr(lst))))       % a+b == a-(0-b)
in (sum list(1,2,3))    
")
  (define src-3     ; car, null? , cdr , list
    "
  let index = proc (n)
                letrec inner (lst)
                             = if null?(lst)
                                  then raise 99
                                  else if zero?(-(car(lst),n))
                                          then 0
                                          else -((inner cdr(lst)), -1)
                in proc (lst)
                        try
                           (inner lst)
                        catch (x)
                           -(x,1)
  in ((index 5) list(2, 3))  
")
  ;````````````````````
  (define src-4
    "
let f3 = proc (x y z) if x then y else z
in (f3 zero?(1) 4 7)
")
  
  (define src-x-5
    "
let f3 = proc (x y z) if x then y else z
in (f3 zero?(1) 4)
")

  (define src-x-6
    "
let a = 1001
in -(x,a)      % 减数大于1000时，抛异常
")
  ;````````````````````
  (define src-beg-0
    "
begin
  print 1 ;
  print 2 ;
  7
end
")
  ;===================================================================================
  (define src-resume-0    ; (num-val -2)
    "try -(3, raise 5)
                    catch (x) 5")
  
  (define src-resume-1    ; (num-val -4)
    "try
         try
           -(3, raise 5)
         catch (x)
           raise 7           % ██████raise 7 提供的 7 会回到 resume 处的  -(3,##) 处， 得到 3-7=-4 作为 内部try-catch语句的结果！
     catch (y)
           y
    ")
 
  ;  > (run src-7)
  ;  ______print____1111111
  ;  ______print____2222222
  ;  End of Computation.
  ;  ($num-val 10002)
  ;  > 
  (define src-7               ; print-exp 的值固定为 10000
    "
  let index = proc (n)
                letrec inner (lst)
                             = if null?(lst)
                                  then begin
                                          raise 99 ;           %███raise-exp的cont为第三次inner cdr(lst) 的cont, 即+1、+1返回
                                          print 2222222
                                       end
                                  else if zero?(-(car(lst),n))
                                          then 0
                                          else -((inner cdr(lst)), -1)
                in proc (lst)
                        try
                           (inner lst)
                        catch (x)
                           begin 
                             -(x,1) ;
                             print 1111111
                           end
  in ((index 5) list(2, 3))  
")

  )
