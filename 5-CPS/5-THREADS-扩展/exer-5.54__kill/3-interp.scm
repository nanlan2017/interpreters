(module interp (lib "eopl.ss" "eopl")
  
  (require "0-lang.scm")
  (require "0-debug.scm")
  (require "0-store.scm")
  (require "1-data-structures.scm")
  (require "1-data-structures-continuation.scm")    
  (require "2-scheduler.scm")
  (require "2-semaphores.scm")     
  
  (provide (all-defined-out))

  (define (apply-procedure/k f argv cont)
    (cases Proc f
      ($procedure (var body saved-env)
                  (eval/k body ($extend-env var (newref argv) saved-env) cont))))

  (define (apply-unop/k unop1 val cont)
    (cases unop unop1
      (zero?-unop () (apply-cont cont ($bool-val (zero? (expval->num val)))))        
      (car-unop () (apply-cont cont (car (expval->list val))))
      (cdr-unop () (apply-cont cont ($list-val (cdr (expval->list val)))))
      (null?-unop () (apply-cont cont ($bool-val (null? (expval->list val)))))
      (print-unop () (begin
                       (eopl:printf "~a~%" (expval->num val))
                       (apply-cont cont ($stub 'PRINT_RET))))
      ))

  (define (run ticks src)
    (value-of-program ticks (scan&parse src)))
  ; ----------------------------------------------------------------------------------------------------
  
  (define fresh-thread-id
    (let [(sn -1)]
      (lambda ()
        (set! sn (+ 1 sn))
        sn)))

  (define (remove-first pred l)
    (let loop ([passedlst '()]
               [lst l])
      (if (null? lst)
          #f
          (let ([head (car lst)]
                [tail (cdr lst)])
            (if (pred head)
                (append (reverse passedlst) tail)
                (loop (cons head passedlst) tail))))))
  
  (define (is-of-id id)
    (lambda (th)
      (= (thread->id th) id)))


  
  ; ==========================================================================================================  spawn时创建新线程（区别于“更新已有线程”）
  ;                                                                                                              end-cont时移除线程
  ; 添加新线程到 %list-of-threads
  ; new-thread :: Procedure -> Thread
  (define (new-thread proc1)
    (let* [(thread-id (fresh-thread-id))
           (th ($a-thread thread-id
                          (lambda () (apply-procedure/k proc1 ($num-val thread-id) ($end-cont-subthread)))
                          #f))]
      (eopl:printf ".......Thread #~s is created and add to $list-of-threads. ~n" thread-id)
      (set-list-of-threads! (cons th %list-of-threads))
      th))

  ; 从 %list-of-threads中查找/移除
  (define (get-thread-from-pool-by-id id)
    (get-thread-by-id-from id %list-of-threads))
  (define (remove-thread-from-pool-by-id id)
    (eopl:printf ".......Thread #~s is removed from $list-of-threads. ~n" id)
    (remove-thread-by-id-from id %list-of-threads))

  ; --------------------------------------------------------------
  ; 从thread的列表中查找/移除
  (define (get-thread-by-id-from id th-lst)
    (let loop [(lst th-lst)]
      (if (null? lst)
          (eopl:error "Didn't find thread of No.~s in pool!~n" id)
          (if (= id (thread->id (car lst)))
              (car lst)
              (loop (cdr lst))))))

  (define (remove-thread-by-id-from id th-lst)
    (let ([result (remove-first (is-of-id id) th-lst)])
      (if result
          (set! th-lst result)
          (eopl:error 'remove-thread-by-id-from "failed to remove thread of id ~s" id))))

  ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< LOG
  (define (remove-duplicates lst)
    (cond ((null? lst) '())
          ((member (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
          (else (cons (car lst) (remove-duplicates (cdr lst))))))
  
  (define (my-filter pred ls) 
    (cond ((null? ls) '())
          ((pred (car ls)) (cons (car ls) (my-filter pred (cdr ls))))
          (else (my-filter pred (cdr ls)))))
  
  ; 获取所有的Mutex-es
  (define (collect-system-mutexes)
    (let* [(maybe-mutexes (map thread->maybe-mutex %list-of-threads))
           (lst0 (remove-duplicates maybe-mutexes))]
      (my-filter Mutex? lst0)))

  (define (mutex-name mut)
    (cases Mutex mut
      ($a-mutex (ref1 ref2)
                (string-append "mutex-" (number->string ref1) "-" (number->string ref2)))))

  (define (get-wait-queue-ids-by-mutex mut)
    (cases Mutex mut
      ($a-mutex (ref1 ref2)
                (map thread->id (deref ref2)))))

  (define (log-system-state)
    ; %current-thread-id
    (eopl:printf "......[%current-thread-id] ~s~n" %current-thread-id)
    ; %ready-queue
    (eopl:printf "......[%ready-queue] ~s~n" (map thread->id %ready-queue))
    ; %list-of-threads
    (eopl:printf "......[%list-of-threads] ~s~n" (map thread->id %list-of-threads))
    ; mutex , wait-queue
    (for-each (lambda (mut)
                (eopl:printf "......[mutex] ~s~n" (mutex-name mut))
                (eopl:printf "......[wait-queue] ~s~n" (get-wait-queue-ids-by-mutex mut)))
              (collect-system-mutexes))
    )
  ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ; 在系统内消除thread
  ; todo : 修改返回值为 #t , #f
  (define (kill-thread-by-id id)
    (eopl:printf "           going to kill thread #~s : ~n" id)
    (eopl:printf "           【state before killing】 : ~n")
    (log-system-state)
    ; %list-of-threads
    (remove-thread-from-pool-by-id id)
    (cases Thread (get-thread-from-pool-by-id id)
      ($a-thread (no proc maybe-mut)
                 (if (= no %current-thread-id)
                     ; 1. current-running
                     (run-next-thread)
                     (if maybe-mut
                         ; 2. wait-queue of mutex
                         (cases Mutex maybe-mut
                           ($a-mutex (ref@closed? ref@wait-queue)
                                     (remove-thread-by-id-from id (deref ref@wait-queue))))
                         ; 3. %ready-queue
                         (remove-thread-by-id-from id %ready-queue)))))
    (eopl:printf "           【state after killing】 : ~n")
    (log-system-state))

  ; ==========================================================================================================
  ; value-of-program : Int * Program -> ExpVal     
  (define (value-of-program timeslice pgm)
    (initialize-store!)    
    (cases program pgm
      (a-program (exp1)
                 (let [(main-thread ($a-thread (fresh-thread-id)
                                               (lambda () (eval/k exp1 (init-env) ($end-cont-main-thread)))
                                               #f))]
                   (initialize-scheduler! timeslice main-thread)
                   (run-thread main-thread)))))
  
  ; value-of/k : Exp * Env * Cont -> FinalAnswer  
  (define (eval/k exp env cont)
    (when (@debug)
      (eopl:printf "           eval/k : ~s~n" (get-exp-variant-name exp)))
      
    (cases expression exp
      (const-exp (num)
                 (apply-cont cont ($num-val num)))
      (var-exp (var)
               (apply-cont cont (deref (apply-env env var))))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      (let-exp (var exp1 body)
               (eval/k (call-exp (proc-exp var body) exp1) env cont))
      (letrec-exp (p-names b-vars p-bodies letrec-body)
                  (eval/k letrec-body ($extend-env-rec* p-names b-vars p-bodies env) cont))
      (const-list-exp (nums)
                      (apply-cont cont ($list-val (map $num-val nums))))
      (diff-exp (exp1 exp2)
                (eval/k exp1 env ($diff1-cont exp2 env cont)))
      (if-exp (exp1 exp2 exp3)
              (eval/k exp1 env ($if-test-cont exp2 exp3 env cont)))
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont)))
      (begin-exp (exp exps)    
                 (if (null? exps)
                     (eval/k exp env cont)
                     (eval/k (call-exp (proc-exp (fresh-identifier 'dummy) (begin-exp (car exps) (cdr exps))) exp) env cont)))
      (set-exp (id exp)
               (eval/k exp env ($set-rhs-cont (apply-env env id) cont)))
      (unop-exp (unop1 exp)
                (eval/k exp env ($unop-arg-cont unop1 cont)))
      ; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
      (spawn-exp (exp)
                 (eval/k exp env ($spawn-cont cont)))
      (mutex-exp ()
                 (apply-cont cont ($mutex-val (new-mutex))))
      (wait-exp (exp)
                (eval/k exp env ($wait-cont cont)))
      (signal-exp (exp)
                  (eval/k exp env ($signal-cont cont)))
      ; ******************************************************************
      ;  spawn(proc)  :  创建一个Thread，并添加到%ready-queue
      ;
      ;  mutex();     :  (new-mutex)
      ;  wait(mut);   :  当前线程前往mutex
      ;  signal(mut); :  当前线程离开mutex
      ;
      ;  yield();     :  手动切换下一个线程
      ;
      ;  kill(..);    :  
      ; ******************************************************************
      ;  【3个重要函数】
      ;    1.  (run-next-thread)
      ;
      ;    2.  (wait-for-mutex)
      ;    3.  (signal-to-mutex)
      ; ******************************************************************
      ;  【全局状态】
      ;    1.  %ready-queue        ：
      ;    2.  %current-thread-id  : 线程切换时自动更新
      ;    3.  %list-of-threads    :  
      ;
      ; ******************************************************************

      (yield-exp ()
                 (place-on-ready-queue! ($a-thread %current-thread-id ; ░░ 线程首尾循环
                                                   (lambda () (apply-cont cont ($stub 'YIELD-RET)))
                                                   #f))
                 (run-next-thread))

      (kill-exp (exp)
                (eval/k exp env ($kill-cont cont)))
      ))
  ; apply-cont : Cont * Exp -> FinalAnswer  
  (define (apply-cont k VAL)
    (when (@debug) (eopl:printf "           apply-cont : ~s~n" (get-cont-variant-name k)))
    
    (if (time-expired?)
        (begin ; time expired !
          (place-on-ready-queue! ($a-thread %current-thread-id  ; ░░ 线程首尾循环
                                            (lambda () (apply-cont k VAL))
                                            #f))
          (run-next-thread))  
        (begin ; time remains ~
          (decrement-timer!)

          (cases Continuation k       
            ($spawn-cont (cont)
                         (let* [(f (expval->proc VAL))
                                (th (new-thread f))]
                           (place-on-ready-queue! th) ; ███                                                
                           (apply-cont cont ($num-val (thread->id th)))))  ; spawn返回值：所创建的线程的 $num-val id

            ($kill-cont (cont)
                        (kill-thread-by-id (expval->num VAL)) 
                        (apply-cont cont ($stub 'KILL-RET)))
            ; ------------------------------ not %ready-queue, but mutex : wait queue
            ($wait-cont (cont)
                        (let [(mut (expval->mutex VAL))]
                          (wait-for-mutex mut ($a-thread %current-thread-id ; ░░█
                                                         (lambda () (apply-cont cont ($stub 'WAIT-RET)))
                                                         mut))))

            ($signal-cont (cont)
                          (let [(mut (expval->mutex VAL))]
                            (signal-to-mutex mut ($a-thread %current-thread-id ; ░░█
                                                            (lambda () (apply-cont cont ($stub 'SIGNAL-RET)))
                                                            #f))))
            ; -----------------------------------------------------------------------------  
            ($end-cont-main-thread ()
                                   (set-final-answer! VAL)
                                   (remove-thread-from-pool-by-id %current-thread-id)
                                   (run-next-thread))  
            ($end-cont-subthread ()
                                 (remove-thread-from-pool-by-id %current-thread-id)
                                 (run-next-thread))

            ; ///////////////////////////////////////////////////////////////////////////////////////////////////
            ; unary-op
            ($unop-arg-cont (unop1 cont)
                            (apply-unop/k unop1 VAL cont))
            ; diff-exp
            ($diff1-cont (exp2 saved-env saved-cont)
                         (eval/k exp2 saved-env ($diff2-cont VAL saved-cont)))
            ($diff2-cont (val1 saved-cont)
                         (let ((n1 (expval->num val1))
                               (n2 (expval->num VAL)))
                           (apply-cont saved-cont ($num-val (- n1 n2)))))
            ; if
            ($if-test-cont (exp2 exp3 env cont)
                           (if (expval->bool VAL)
                               (eval/k exp2 env cont)
                               (eval/k exp3 env cont)))
            ; call-exp
            ($rator-cont (rand saved-env saved-cont)
                         (eval/k rand saved-env ($rand-cont VAL saved-cont)))
            ($rand-cont (val1 saved-cont)
                        (let ((f (expval->proc val1)))
                          (apply-procedure/k f VAL saved-cont)))
            ; set
            ($set-rhs-cont (loc cont)
                           (begin
                             (setref! loc VAL)
                             (apply-cont cont ($stub 'SET-RET))))
            ))))

  )

