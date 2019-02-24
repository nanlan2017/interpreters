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

  )
