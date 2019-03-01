(module testcases-Statement-oriented (lib "eopl.ss" "eopl")
  (require "0-lang.scm")
  (require "1-store.scm")
  (require "2-interp.scm")
  (provide (all-defined-out))


  (define src-1 ;7
    "var x,y; {x = 3; y = 4; print +(x,y)}"
    )
  (define src-2 ; 12
    "var x,y,z; {x = 3;
                 y = 4;
                 z = 0;
                 while not(zero?(x))
                    {z = +(z,y);
                     x = -(x,1)};
                 print z}"
    )
  (define src-3 ; 3 4 3
    " var x; {x = 3;
              print x;
              var x; {x = 4; print x};
              print x}"
    )
  (define src-4   ; 12
    "var f,x; {f = proc(x) *(x,4);
               x = 3;
               print (f x)}"
    )
  )
