(module testcases-CLASSES (lib "eopl.ss" "eopl")
  (require "0-lang.scm")
  (require "0-store.scm")
  
  (require "1-data-structures.scm")
  (require "1-classes.scm")
  (require "2-interp.scm")
  
  (provide (all-defined-out))
  ;==============================================================
  (define src-0
    "
  class c1 extends object
    field i
    field j
    method initialize (x)
      begin
       set i = x;
       set j = -(0,x)
      end
    method countup (d)
      begin
       set i = +(i,d);
       set j = -(j,d)
      end
    method getstate ()
      list(i,j)
   %---------------------
  let t1 = 0
      t2 = 0
      o1 = new c1(3)
  in begin
      set t1 = send o1 getstate();
      send o1 countup(2);
      set t2 = send o1 getstate();
      list(t1,t2)
     end
  ")


  (define src-1
    "
  class interior-node extends object
      field left
      field right
      method initialize (l, r)
        begin
          set left = l;
          set right = r
        end
      method sum () +(send left sum(),send right sum())
  %--------------------
  class leaf-node extends object
     field value
     method initialize (v) set value = v
     method sum () value
  %--------------------
  let o1 = new interior-node(
             new interior-node(
               new leaf-node(3),
               new leaf-node(4)),
             new leaf-node(5))
  in send o1 sum()
  ")


  (define src-self-super
    "
class c1 extends object
  method initialize () 1
  method m1 () send self m2()
  method m2 () 13
class c2 extends c1
  method m1 () 22
  method m2 () 23
  method m3 () super m1()
class c3 extends c2
  method m1 () 32
  method m2 () 33
let o3 = new c3()
in send o3 m3()
")
  )
