(module test-implicit-ref (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  ;;========================================================

  (define src-2
    "let g = let count = 0
             in proc (dummy)
                   begin
                       set count = -(count,-9);
                       count
                   end
     in let a = (g 11)
        in let b = (g 11)
           in -(a,b)"
    )

  ;; 测试 call-by-value
  (define src-3
    "let dec = proc (x) -(x,1)
     in  let y = 10
         in begin
              (dec y) ;
              y                  % 此处y值为10，则call-by-value ; 为9, 则call-by-reference
            end
     ")
  (define src-3a
    "
  let
     p = proc (x) set x = 4
  in
     let
         a = 3
     in
         begin (p a); a end
")

  ;; set 可定义递归函数
  (define src-4
    "
  let times4 = 0
  in begin
         set times4 = proc (x) if zero?(x) then 0 else -((times4 -(x,1)), -4);
         (times4 3)
     end
")

  ;; mutable pair
  (define src-5   ;; 88
  "
  let glo = pair(11,22)
  in let f = proc (loc) let d1 = setright(loc, left(loc))
                        in let d2 = setleft(glo, 99)
                           in -(left(loc),right(loc))
     in (f glo)
")



  )