(module semaphores (lib "eopl.ss" "eopl")

  (require "0-store.scm")              
  (require "1-data-structures.scm")       
  (require "2-scheduler.scm")         
  (require "1-queues.scm")
  (provide (all-defined-out))
  ; ------------------------------------------------------------------------- 
  ;                                                                        %ready-queue和 %list-of-threads中改为 Thread类型
  
  ; new-mutex () -> Mutex
  (define (new-mutex)
    ($a-mutex
     (newref #f)
     (newref '())))
  
  ; wait-for-mutex : Mutex * Thread -> FinalAnswer
  (define (wait-for-mutex m th-c)
    (cases Mutex m
      ($a-mutex (ref@closed? ref@wait-queue)
                (let [(closed? (deref ref@closed?))
                      (wait-queue (deref ref@wait-queue))]
                  ; -------------- 并未涉及Thread的具体结构
                  (cond
                    [closed? (setref! ref@wait-queue (enqueue wait-queue th-c))   ; 从%ready-queue 进入 wait-queue
                             (run-next-thread)]
                    
                    [else (setref! ref@closed? #t)
                          (run-thread th-c)]
                    )))))
  
  ; signal-mutex : Mutex * Thread -> FinalAnswer
  (define (signal-to-mutex m th-c)
    (cases Mutex m
      ($a-mutex (ref@closed? ref@wait-queue)
                (let [(closed? (deref ref@closed?))
                      (wait-queue (deref ref@wait-queue))]
                  ; -------------- 并未涉及Thread的具体结构
                  ;  a signal operation always succeeds: the thread that executes it remains the running thread.
                  (when closed? (if (empty? wait-queue)
                                    (setref! ref@closed? #f)
                                    (dequeue wait-queue (lambda (first-waiting-th other-waiting-ths)
                                                          (place-on-ready-queue! first-waiting-th)   ; 从%wait-queue回到%ready queue 【说明2者数据结构一致】
                                                          (setref! ref@wait-queue other-waiting-ths)))))
                  (run-thread th-c)))))

  )
