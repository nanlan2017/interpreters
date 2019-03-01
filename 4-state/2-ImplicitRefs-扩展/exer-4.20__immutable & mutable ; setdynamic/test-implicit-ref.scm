(module test-implicit-ref (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "0-lang.scm")
  (require "1-data-structures.scm")
  (require "2-interp.scm")
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

  ;; set 可定义递归函数
  (define src-4
    "
  let times4 = 0
  in begin
         set times4 = proc (x) if zero?(x) then 0 else -((times4 -(x,1)), -4);
         (times4 3)
     end
")

  ;`````````````````````````````````` immutable
  (define src-5
    "
letmutable a = 3
in letmutable m_b = 11
   in begin  set a = 101;
             set m_b = 102;
             -(a,m_b)
      end
")
  ;`````````````````````````````````` dynamic assignment/ fluid binding
  (define src-6
    "
letmutable x = 11
in let p = proc (y) -(y,x)
in -(setdynamic x = 17 during (p 22),    % 22-17=5
(p 13))                                  % 13-11=2   ==> 5-2=3
      ")



  )