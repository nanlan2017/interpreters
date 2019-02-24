(module semaphores (lib "eopl.ss" "eopl")

  (require "0-store.scm")              
  (require "1-data-structures.scm")       
  (require "2-scheduler.scm")         
  (require "1-queues.scm")
  (provide (all-defined-out))
  ; implements binary semaphores (mutexes).
  ; ------------------------------------------------------------------------- 
  ; new-mutex () -> Mutex
  (define (new-mutex)
    ($a-mutex
     (newref #f)
     (newref '())))
  
  ; wait-for-mutex : Mutex * Thread -> FinalAnswer
  (define (access-mutex m th)
    (cases Mutex m
      ($a-mutex (ref@closed? ref@wait-queue)
                (let [(closed? (deref ref@closed?))
                      (wait-queue (deref ref@wait-queue))]
                  (cond
                    ; 该锁锁上 : 则将此线程加入wait quene。
                    ; 该锁开放 : 则运行本线程、关锁
                    [closed? (setref! ref@wait-queue (enqueue wait-queue ($a-thread th %MaxTimeSlice)))
                             (run-next-thread)]
                    
                    [else (setref! ref@closed? #t)
                          (th)]
                    )))))

  
  ; signal-mutex : Mutex * Thread -> FinalAnswer
  (define (leave-mutex m th)
    (cases Mutex m
      ($a-mutex (ref@closed? ref@wait-queue)
                (let [(closed? (deref ref@closed?))
                      (wait-queue (deref ref@wait-queue))]
                  ; 取决于：1. closed?  2. wait quene 是否empty?
                  ; A :  closed + empty     ==>  本线程接着跑；开锁
                  ; B :  closed + Non-empty ==>  本线程接着跑；把wait quene的头线程移入%ready-quene
                  ; C :  opened             ==>  本线程接着跑
                  ; =====>  a signal operation always succeeds: the thread that executes it remains the running thread.
                  (when closed? (if (empty? wait-queue)
                                    (setref! ref@closed? #f)
                                    (dequeue wait-queue (lambda (first-waiting-th other-waiting-ths)
                                                          (place-on-ready-queue! first-waiting-th)
                                                          (setref! ref@wait-queue other-waiting-ths)))))
                  (th)))))

  )
