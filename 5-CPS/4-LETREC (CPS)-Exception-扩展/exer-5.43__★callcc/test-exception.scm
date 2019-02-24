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
  ; ==================================== letcc, throw
  (define src-cc-0
    "letcc ct in 99"
    )
  
  (define src-cc-1
    "letcc ct in ct"
    )
  
  (define src-cc-2    ; 13-5 = 8
    "try
       try
         -(13, letcc x in raise x)
       catch (x)
         raise x
    catch (y)
       throw 5 to y"
    )
  (define src-cc-3
    "  try
         -(13, letcc x in raise x)
       catch (x)
         throw 5 to x
")
  ;====================================== call/cc
  ; 说白了：  call/cc k 只要body里使用了k, 就把
  (define src-call-0        ; 100-4 = 96
    "
-(100,[call/cc (lambda (k)
                 -(5,(k 4)))
               ])
")
  (define src-call-1
    "
  let x = [call/cc (lambda (k)
                     k)]
  in (x proc (ignore) 5)
  ")
  )

