(module interp (lib "eopl.ss" "eopl")
  
  (require "0-lang.scm")
  (require "0-debug.scm")
  (require "0-store.scm")
  (require "1-data-structures.scm")
  (require "1-data-structures-continuation.scm")    
  (require "2-scheduler.scm")
  (require "2-semaphores.scm")     
  
  (provide run
           value-of-program
           Option@trace-interp

           ; imported
           get-store-as-list
           
           %ready-queue
           %final-answer
           %max-time-slice
           %time-remaining
           )
  ; ********************************************************************************************************
  ; 由于 Expression从语义上来讲都应该是有val的,在此赋予这些 executed for effect 的expression一些约定的任意值
  ; 包括：
  ;   1.  set
  ;   2.  print
  ;   3.  spawn , wait , signal , yield
  ; ████████统一使用 $stub 'xx-RET
  ; ********************************************************************************************************

  ; setting option
  (define Option@trace-interp (make-parameter #f))

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

  ; value-of-program : Int * Program -> ExpVal     
  (define (value-of-program timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
                 (eval/k exp1 (init-env) ($end-cont-main-thread)))))
  ; ==========================================================================================================
  ; Page 182
  ; value-of/k : Exp * Env * Cont -> FinalAnswer  
  (define (eval/k exp env cont)
    (when (Option@trace-interp)
      (eopl:printf "value-of/k: ~s~%" exp))
      
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
      ; `````````````````````````````````````````
      (diff-exp (exp1 exp2)
                (eval/k exp1 env ($diff1-cont exp2 env cont)))

      (if-exp (exp1 exp2 exp3)
              (eval/k exp1 env ($if-test-cont exp2 exp3 env cont)))

      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont)))

      (begin-exp (exp exps)    
                 (if (null? exps)
                     (eval/k exp env cont)
                     (eval/k (call-exp (proc-exp (fresh-identifier 'dummy) (begin-exp (car exps) (cdr exps))) exp) env cont)
                     ; (eval/k (begin-exp (car exps) (cdr exps)) env cont)  ; 错了！只有最后一个exp被求值了！
                     ))

      (set-exp (id exp)
               (eval/k exp env ($set-rhs-cont (apply-env env id) cont)))

      (unop-exp (unop1 exp)
                (eval/k exp env ($unop-arg-cont unop1 cont)))
      ; -------------------------------------------------------
      (spawn-exp (exp)
                 (eval/k exp env ($spawn-cont cont)))

      ; ----------------
      (mutex-exp ()
                 (apply-cont cont ($mutex-val (new-mutex))))

      (wait-exp (exp)
                (eval/k exp env ($wait-cont cont)))

      (signal-exp (exp)
                  (eval/k exp env ($signal-cont cont)))
      ; ----------------
      (yield-exp ()
                 ; ☆ yield是挂起当前线程（同样是推进%ready-quene） （运行下一个线程）
                 (place-on-ready-queue! (lambda ()
                                          (apply-cont cont ($stub 'YIELD-RET))))
                 (run-next-thread))

        
      ))
  ; ===============================================================
  ; Page: 182 and 186
  ; apply-cont : Cont * Exp -> FinalAnswer  
  (define (apply-cont k VAL)
    (when (@debug) (eopl:printf "               apply-cont~n"))
    (if (time-expired?)
        ; time expired !           ; ██████ 进行线程切换！
        (begin
          (place-on-ready-queue! (lambda ()
                                   (apply-cont k VAL)))
          (run-next-thread))
        ; time remains ~
        (begin
          (decrement-timer!)       ; ██████ 唯一的 decrement-timer !  apply-cont就是“计步”的地方，因为Monad化的每一个链条就代表一个计算步骤  f x >>=\v1-> g y >>=\v2-> ...
            
          (cases Continuation k
            ($end-cont-main-thread ()
                                   (set-final-answer! VAL)
                                   (run-next-thread))
  
            ($end-cont-subthread ()
                                 (run-next-thread))
            ; ------------------------------------------------------------
            
            ($spawn-cont (cont)
                         (let ((f (expval->proc VAL)))
                           ; ☆ spawn : 只是把一个线程放入 %ready-quene， 并不会立即运行！
                           (place-on-ready-queue! (lambda ()
                                                    (apply-procedure/k f ($stub 'DUMMY-ARG) ($end-cont-subthread))))
                           (apply-cont cont ($stub 'SPAWN-RET))))
            ; ---------------------
            ($wait-cont (cont)
                        (let [(mut (expval->mutex VAL))]
                          (access-mutex mut (lambda ()
                                              (apply-cont cont ($stub 'WAIT-RET))))))

            ($signal-cont (cont)
                          (let [(mut (expval->mutex VAL))]
                            (leave-mutex mut (lambda ()
                                               (apply-cont cont ($stub 'SIGNAL-RET))))))

            ; ------------------------------------------------------------
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

