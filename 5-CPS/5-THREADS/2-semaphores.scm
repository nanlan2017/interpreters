(module semaphores (lib "eopl.ss" "eopl")

  (require "0-store.scm")                    ; for store ops
  (require "1-data-structures.scm")          ; for lock, a-lock (Mutex)
  (require "2-scheduler.scm")                ; for os calls
  (require "1-queues.scm")

  (provide (all-defined-out))
  ; implements binary semaphores (mutexes).

  (define Option@-instrument-mutexes (make-parameter #f))
  ; ------------------------------------------------------------------------- 
  ; new-mutex () -> Mutex
  (define (new-mutex)
    ($a-mutex
     (newref #f)
     (newref '())))

  ;**************************************************************************
  ; wait :
  ; a unary operation by which a thread indicates that it wants access to a mutex.
  ; Its argument must be a mutex. Its behavior depends on the state of the mutex

  ; signal :
  ; a unary operation by which a thread indicates that it is ready to release a mutex

  ;░░░░░░ These properties guarantee that only one thread can execute between a successive pair of calls to wait and signal.
  ;░░░░░░ This portion of the program is called a critical region.
  ;░░░░░░ It is impossible for two different threads to be concurrently excecuting code in a critical region

  ;  let mut1 = (new-mutex)
  ;  ....
  ;  wait(mut1);
  ;  #
  ;  # 'region'    <------------- only one thread can execute the code region at a time
  ;  #
  ;  signal(mut1);
  
  ;**************************************************************************
  
  ; wait-for-mutex : Mutex * Thread -> FinalAnswer
  (define (access-mutex m trd)
    (cases Mutex m
      ($a-mutex (ref@closed? ref@wait-queue)
                (let [(closed? (deref ref@closed?))
                      (wait-queue (deref ref@wait-queue))]
                  (cond
                    ; 该锁锁上 : 则将此线程加入wait quene。 ███ We say that the current thread is blocked waiting for this mutex
                    [closed? (setref! ref@wait-queue (enqueue wait-queue trd))
                             (run-next-thread)]
                    ; 该锁开放 : 则运行本线程
                    [else (setref! ref@closed? #t)  ; 关锁
                          (trd)]
                    )))))

  
  ;; signal-mutex : Mutex * Thread -> FinalAnswer
  ;; Page 190
  (define (leave-mutex m th)
    (cases Mutex m
      ($a-mutex (ref@closed? ref@wait-queue)
                (let [(closed? (deref ref@closed?))
                      (wait-queue (deref ref@wait-queue))]
                  ; 取决于：1. closed?  2. wait quene 是否empty?
                  ; A :  closed + empty     ==>  本线程接着跑；开锁、
                  ; B :  closed + Non-empty ==>  本线程接着跑；把wait quene的头线程移入%ready-quene
                  ; C :  opened             ==>  本线程接着跑
                  (when closed? (if (empty? wait-queue)
                                    (setref! ref@closed? #f)    ; 开锁
                                    (dequeue wait-queue (lambda (first-waiting-th other-waiting-ths)
                                                          (place-on-ready-queue! first-waiting-th)
                                                          (setref! ref@wait-queue other-waiting-ths)))))
                  (th)))))

  )
