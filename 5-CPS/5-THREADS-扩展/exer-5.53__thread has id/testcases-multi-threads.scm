(module testcases-multi-threads (lib "eopl.ss" "eopl")
  (require "0-debug.scm")
  (require "0-lang.scm")
  (require "0-store.scm")
  (require "3-interp.scm")
  ; ===============================================

  ;  两个互相独立不协作的线程
  (define src-2-th
    "
letrec
   noisy (l) = if null?(l) then 0 else begin print(car(l)); (noisy cdr(l)) end
in
  begin
     spawn(proc (dummy) (noisy [1,2,3,4,5])) ;
     spawn(proc (dummy) (noisy [6,7,8,9,10])) ;
     print(100);
     33
  end
")
  ; 两个相互协作的线程： producer/consumer
  (define src-2-cop
    "
    let buffer = 0
    in let producer = proc (n)
            letrec waitt(k) = if zero?(k)
                             then set buffer = n
                             else begin
                                      print(-(k,-200));
                                      (waitt -(k,1))
                                  end
            in (waitt 5)
       in let consumer = proc (d)
                letrec busywait (k) = if zero?(buffer)
                                      then begin
                                             print(-(k,-100));
                                             (busywait -(k,-1))
                                           end
                                      else buffer
                in (busywait 0)
          in begin
              spawn(proc (d) (producer 44));
              print(300);
              (consumer 86)
             end
    "
    )
  (define src-2-cop-s
    "let buffer = 0
     in let mut = mutex()
      in let producer = proc (n)
                          letrec wait1(k) = if zero?(k)
                                            then begin set buffer = n;
                                                       signal(mut)
                                                 end
                                            else begin print(-(k, -200));
                                                       (wait1 -(k, 1))
                                                 end
                          in (wait1 5)
         in let consumer = proc (d)
                             begin wait(mut);
                                   buffer
                             end
            in begin wait(mut);
                     spawn(proc (d) (producer 44));
                     print(300);
                     (consumer 86)
               end")
  ; =====================================================
  (define src-bad
    "
let x = 0
in let incr_x = proc (id)
                   proc (dummy)
                     set x = -(x,-1)
   in begin
        spawn((incr_x 100));
        spawn((incr_x 200));
        spawn((incr_x 300))
      end
")

  (define src-mutex
    "
let x = 0
in let mut1 = mutex()
in let incr_x = proc (id)
                   proc (dummy)
                      begin
                        wait(mut1) ;
                        set x = -(x,-1) ;               %██████████ 此region被 wait~signal 包围起来！
                        signal(mut1)
                      end
   in begin
        spawn((incr_x 100));
        spawn((incr_x 200));
        spawn((incr_x 300))
      end
")

  (define src-mutex-2
  "let x = 0
   in let mut = mutex()
      in let incr_x = proc (id)
                        let mut1 = mutex()
                        in begin wait(mut1);
                                 spawn(proc (dummy)
                                         begin wait(mut);
                                               set x = -(x, -1);
                                               signal(mut);
                                               signal(mut1)
                                         end);
                                 mut1
                           end
         in let mut1 = (incr_x 100)
            in let mut2 = (incr_x 200)
               in let mut3 = (incr_x 300)
                  in begin wait(mut1);
                           wait(mut2);
                           wait(mut3);
                           x
                     end")

  )
